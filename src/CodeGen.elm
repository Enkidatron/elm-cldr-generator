module CodeGen exposing (mainLocaleFile)

import Elm.CodeGen as Gen
import Elm.Pretty
import Internal.Structures exposing (EraNames, MonthNames, Patterns, WeekdayNames)
import LanguageInfo exposing (LanguageInfo, skewerCase, snakeIdentifier)
import Parser exposing ((|.), (|=), Parser)
import Set


localeAnn : Gen.TypeAnnotation
localeAnn =
    Gen.typed "Locale" []


mainLocaleFile : List ( String, List LanguageInfo ) -> String
mainLocaleFile groupedInfos =
    let
        infos =
            List.concatMap Tuple.second groupedInfos
    in
    Gen.file
        (Gen.normalModule [ "Cldr", "Locale" ]
            (infos
                |> List.map snakeIdentifier
                |> (++) [ "fromString", "allLocales", "basicLocales", "toUnicode" ]
                |> List.map Gen.funExpose
                |> (::) (Gen.closedTypeExpose "Locale")
            )
        )
        [ Gen.importStmt [ "DateFormat" ] Nothing (Just Gen.exposeAll)
        , Gen.importStmt [ "Internal", "Locale" ]
            Nothing
            (Just
                (Gen.exposeExplicit
                    [ Gen.openTypeExpose "DateTimeToken"
                    , Gen.closedTypeExpose "Internal"
                    , Gen.openTypeExpose "LanguageId"
                    , Gen.openTypeExpose "TimeToken"
                    , Gen.funExpose "normalize"
                    ]
                )
            )
        , Gen.importStmt [ "Tagged" ]
            Nothing
            (Just
                (Gen.exposeExplicit [ Gen.openTypeExpose "Tagged" ])
            )
        ]
        (mainLocaleDeclarations infos)
        (Just (mainLocaleFileComment groupedInfos))
        |> Elm.Pretty.pretty 80


mainLocaleFileComment : List ( String, List LanguageInfo ) -> Gen.Comment Gen.FileComment
mainLocaleFileComment groupedInfos =
    Gen.emptyFileComment
        |> Gen.markdown "# Locale"
        |> Gen.docTags [ "Locale" ]
        |> Gen.markdown "## Create"
        |> Gen.docTags [ "fromString" ]
        |> Gen.markdown ""
        |> Gen.docTags [ "allLocales, basicLocales" ]
        |> Gen.markdown "## Convert"
        |> Gen.docTags [ "toUnicode" ]
        |> Gen.markdown "## Locales by language"
        |> (\comment -> List.foldl addDocForLangGroup comment groupedInfos)


addDocForLangGroup : ( String, List LanguageInfo ) -> Gen.Comment Gen.FileComment -> Gen.Comment Gen.FileComment
addDocForLangGroup ( langTag, infos ) =
    Gen.markdown ("### " ++ langTag)
        >> Gen.docTags (List.map snakeIdentifier infos)


mainLocaleDeclarations : List LanguageInfo -> List Gen.Declaration
mainLocaleDeclarations infos =
    [ localeTypeDeclaration
    , toUnicodeDeclaration
    , fromStringDeclaration
    , allLocalesDeclarationForMainLocale infos
    , basicLocalesDeclarationForMainLocale infos
    ]
        ++ List.map localeFileDeclaration infos


localeTypeDeclaration : Gen.Declaration
localeTypeDeclaration =
    Gen.aliasDecl
        (Just
            (Gen.emptyDocComment
                |> Gen.markdown "Represents the locale to use for formatting."
                |> Gen.markdown "You can parse a value from JS like `navigator.language` or use hardcoded locales."
            )
        )
        "Locale"
        []
        (Gen.fqTyped [ "Internal", "Locale" ] "Locale" [])


toUnicodeDeclaration : Gen.Declaration
toUnicodeDeclaration =
    Gen.funDecl (Just toUnicodeDocComment)
        (Just (Gen.funAnn localeAnn Gen.stringAnn))
        "toUnicode"
        [ Gen.varPattern "locale" ]
        (Gen.apply
            [ Gen.fqFun [ "Internal", "Locale" ] "toUnicode"
            , Gen.val "locale"
            ]
        )


toUnicodeDocComment : Gen.Comment Gen.DocComment
toUnicodeDocComment =
    Gen.emptyDocComment
        |> Gen.markdown "Get the [Unicode](https://unicode.org/reports/tr35/#Identifiers) representation of a locale."
        |> Gen.code "toUnicode en_GB"
        |> Gen.code "--> \"en-GB\""


fromStringDeclaration : Gen.Declaration
fromStringDeclaration =
    Gen.funDecl (Just fromStringDocComment)
        (Just (Gen.funAnn (Gen.listAnn localeAnn) (Gen.funAnn Gen.stringAnn (Gen.maybeAnn localeAnn))))
        "fromString"
        [ Gen.varPattern "candidateLocales" ]
        (Gen.chain
            (Gen.fqFun [ "Internal", "Locale" ] "languageIdFromString")
            [ Gen.apply
                [ Gen.fqFun [ "Maybe" ] "andThen"
                , Gen.parens
                    (Gen.apply
                        [ Gen.fqFun [ "Internal", "Locale" ] "matchNearestLocale"
                        , Gen.val "candidateLocales"
                        ]
                    )
                ]
            ]
        )


fromStringDocComment : Gen.Comment Gen.DocComment
fromStringDocComment =
    Gen.emptyDocComment
        |> Gen.markdown "Parse a `Locale` from a Unicode or BCP47 identifier."
        |> Gen.code "fromString basicLocales \"en\""
        |> Gen.code "--> Just en"
        |> Gen.code ""
        |> Gen.code "fromString allLocales \"en-GB\""
        |> Gen.code "--> Just en_GB"


allLocalesDeclarationForMainLocale : List LanguageInfo -> Gen.Declaration
allLocalesDeclarationForMainLocale infos =
    Gen.funDecl (Just allLocalesDocComment)
        (Just (Gen.listAnn localeAnn))
        "allLocales"
        []
        (Gen.list (List.map (snakeIdentifier >> Gen.val) infos))


allLocalesDocComment : Gen.Comment Gen.DocComment
allLocalesDocComment =
    Gen.emptyDocComment
        |> Gen.markdown "A list of every locale listed in the JSON version of the CLDR."


basicLocalesDeclarationForMainLocale : List LanguageInfo -> Gen.Declaration
basicLocalesDeclarationForMainLocale infos =
    Gen.funDecl (Just basicLocalesDocComment)
        (Just (Gen.listAnn localeAnn))
        "basicLocales"
        []
        (Gen.list (List.filterMap (basicLanguage >> Maybe.map (snakeIdentifier >> Gen.val)) infos))


basicLocalesDocComment : Gen.Comment Gen.DocComment
basicLocalesDocComment =
    Gen.emptyDocComment
        |> Gen.markdown "A list of every \"basic\" locale listed in the JSON version of the CLDR."
        |> Gen.markdown "A \"basic\" locale is a locale without a region, script, or variant subtag, such as `en` or `ru`."


localeFileDeclaration : LanguageInfo -> Gen.Declaration
localeFileDeclaration info =
    Gen.funDecl
        (Just (commentForLanguage info))
        (Just (Gen.typed "Locale" []))
        (snakeIdentifier info)
        []
        (Gen.apply
            [ Gen.fqFun [ "Internal", "Locale" ] "Locale"
            , generatedLangExpression info
            ]
        )


commentForLanguage : LanguageInfo -> Gen.Comment Gen.DocComment
commentForLanguage info =
    Gen.emptyDocComment
        |> Gen.markdown
            (String.concat
                [ "Date format strings:"
                , "\n- Short : "
                , info.datePatterns.short
                , "\n- Medium : "
                , info.datePatterns.medium
                , "\n- Long : "
                , info.datePatterns.long
                , "\n- Full : "
                , info.datePatterns.full
                , "\n\nTime format strings:"
                , "\n- Short : "
                , info.timePatterns.short
                , "\n- Medium : "
                , info.timePatterns.medium
                , "\n- Long : "
                , info.timePatterns.long
                , "\n- Full : "
                , info.timePatterns.full
                ]
            )


basicLanguage : LanguageInfo -> Maybe LanguageInfo
basicLanguage info =
    case ( info.script, info.territory, info.variant ) of
        ( Nothing, Nothing, Nothing ) ->
            Just info

        _ ->
            Nothing


generatedLangExpression : LanguageInfo -> Gen.Expression
generatedLangExpression info =
    Gen.record
        [ ( "languageId"
          , if info.language == "root" then
                Gen.fun "Root"

            else
                Gen.apply
                    [ Gen.fun "Lang"
                    , taggedStringExpr info.language
                    , maybeTaggedStringExpr info.script
                    , maybeTaggedStringExpr info.territory
                    , maybeTaggedStringExpr info.variant
                    ]
          )
        , ( "amPmNames"
          , Gen.record
                [ ( "am", Gen.string info.amPmNames.am )
                , ( "pm", Gen.string info.amPmNames.pm )
                ]
          )
        , ( "datePatterns", patternExpr Gen.string info.datePatterns )
        , ( "monthNames", monthNamesExpr info.monthNames )
        , ( "monthNamesShort", monthNamesExpr info.monthNamesShort )
        , ( "weekdayNames", weekdayNamesExpr info.weekdayNames )
        , ( "weekdayNamesShort", weekdayNamesExpr info.weekdayNamesShort )
        , ( "dateTokens", patternExpr dateFormatTokenBestEffortExpr info.datePatterns )
        , ( "timeTokens", patternExpr dateFormatTokenBestEffortExpr info.timePatterns )
        , ( "dateTimeTokens", patternExpr dateTimeTokenBestEffortExpr info.dateTimePatterns )
        , ( "eraNames", eraNamesExpr info.eraNames )
        ]


taggedStringExpr : String -> Gen.Expression
taggedStringExpr words =
    Gen.parens
        (Gen.apply
            [ Gen.fqFun [ "Tagged" ] "tag"
            , Gen.string words
            ]
        )


maybeTaggedStringExpr : Maybe String -> Gen.Expression
maybeTaggedStringExpr maybeWords =
    case maybeWords of
        Just words ->
            Gen.parens
                (Gen.apply
                    [ Gen.fun "Just"
                    , taggedStringExpr words
                    ]
                )

        Nothing ->
            Gen.fun "Nothing"


patternExpr : (a -> Gen.Expression) -> Patterns a -> Gen.Expression
patternExpr innerExpr pattern =
    Gen.record
        [ ( "short", innerExpr pattern.short )
        , ( "medium", innerExpr pattern.medium )
        , ( "long", innerExpr pattern.long )
        , ( "full", innerExpr pattern.full )
        ]


monthNamesExpr : MonthNames -> Gen.Expression
monthNamesExpr names =
    Gen.record
        [ ( "jan", Gen.string names.jan )
        , ( "feb", Gen.string names.feb )
        , ( "mar", Gen.string names.mar )
        , ( "apr", Gen.string names.apr )
        , ( "may", Gen.string names.may )
        , ( "jun", Gen.string names.jun )
        , ( "jul", Gen.string names.jul )
        , ( "aug", Gen.string names.aug )
        , ( "sep", Gen.string names.sep )
        , ( "oct", Gen.string names.oct )
        , ( "nov", Gen.string names.nov )
        , ( "dec", Gen.string names.dec )
        ]


weekdayNamesExpr : WeekdayNames -> Gen.Expression
weekdayNamesExpr names =
    Gen.record
        [ ( "sun", Gen.string names.sun )
        , ( "mon", Gen.string names.mon )
        , ( "tue", Gen.string names.tue )
        , ( "wed", Gen.string names.wed )
        , ( "thu", Gen.string names.thu )
        , ( "fri", Gen.string names.fri )
        , ( "sat", Gen.string names.sat )
        ]


eraNamesExpr : EraNames -> Gen.Expression
eraNamesExpr names =
    Gen.record
        [ ( "bc", Gen.string names.bc )
        , ( "ad", Gen.string names.ad )
        ]


dateFormatTokenBestEffortExpr : String -> Gen.Expression
dateFormatTokenBestEffortExpr =
    Parser.run dateTimeFormatTokenListParser
        >> Result.map Gen.list
        >> Result.withDefault (Gen.val "ERROR")


dateTimeFormatTokenListParser : Parser (List Gen.Expression)
dateTimeFormatTokenListParser =
    Parser.succeed identity
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.succeed ()
            , item = dateTimeFormatTokenParser
            , trailing = Parser.Optional
            }
        |. Parser.end


parseWord : String -> Gen.Expression -> Parser Gen.Expression
parseWord word expr =
    Parser.map (\_ -> expr) (Parser.symbol word)


dateTimeFormatTokenParser : Parser Gen.Expression
dateTimeFormatTokenParser =
    let
        parseToken : String -> String -> Parser Gen.Expression
        parseToken word funName =
            parseWord word (Gen.apply [ Gen.fun "DF", Gen.fun funName ])

        textExpr : String -> Gen.Expression
        textExpr word =
            Gen.apply
                [ Gen.fun "DF"
                , Gen.parens (Gen.apply [ Gen.fun "text", Gen.string word ])
                ]
    in
    Parser.oneOf
        [ parseToken "dd" "dayOfMonthFixed"
        , parseToken "d" "dayOfMonthNumber"
        , parseToken "EEEE" "dayOfWeekNameFull"
        , parseToken "cccc" "dayOfWeekNameFull"
        , parseToken "MMMM" "monthNameFull"
        , parseToken "MMM" "monthNameAbbreviated"
        , parseToken "MM" "monthFixed"
        , parseToken "M" "monthNumber"
        , parseToken "yy" "yearNumberLastTwo"
        , parseToken "y" "yearNumber"
        , parseWord "G" (Gen.fun "EraAbbr")
        , parseToken "HH" "hourMilitaryFixed"
        , parseToken "H" "hourMilitaryNumber"
        , parseToken "hh" "hourFixed"
        , parseToken "h" "hourNumber"
        , parseToken "mm" "minuteFixed"
        , parseToken "m" "minuteNumber"
        , parseToken "ss" "secondFixed"
        , parseToken "s" "secondNumber"
        , parseToken "a" "amPmUppercase"
        , parseWord "zzzz" (Gen.fun "TimeZoneFull")
        , parseWord "z" (Gen.fun "TimeZoneShort")
        , parseWord "B" (textExpr "") -- "B" is "flexible day periods", which is beyond me at this time
        , Parser.variable
            { start = \c -> not (Char.isAlpha c) && c /= '\''
            , inner = \c -> not (Char.isAlpha c) && c /= '\''
            , reserved = Set.empty
            }
            |> Parser.map textExpr
        , Parser.succeed textExpr
            |. Parser.symbol "'"
            |= Parser.getChompedString (Parser.chompUntil "'")
            |. Parser.symbol "'"
        ]


dateTimeTokenBestEffortExpr : String -> Gen.Expression
dateTimeTokenBestEffortExpr =
    Parser.run dateTimeTokenListParser
        >> Result.map Gen.list
        >> Result.withDefault (Gen.val "ERROR")


dateTimeTokenListParser : Parser (List Gen.Expression)
dateTimeTokenListParser =
    Parser.succeed identity
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.succeed ()
            , item = dateTimeTokenParser
            , trailing = Parser.Optional
            }
        |. Parser.end


dateTimeTokenParser : Parser Gen.Expression
dateTimeTokenParser =
    let
        textExpr : String -> Gen.Expression
        textExpr word =
            Gen.apply [ Gen.fun "Text", Gen.string word ]
    in
    Parser.oneOf
        [ parseWord "{1}" (Gen.fun "DateGoesHere")
        , parseWord "{0}" (Gen.fun "TimeGoesHere")
        , Parser.variable
            { start = \c -> not (Char.isAlpha c) && c /= '\'' && c /= '{'
            , inner = \c -> not (Char.isAlpha c) && c /= '\'' && c /= '{'
            , reserved = Set.empty
            }
            |> Parser.map textExpr
        , Parser.succeed textExpr
            |. Parser.symbol "'"
            |= Parser.getChompedString (Parser.chompUntil "'")
            |. Parser.symbol "'"
        ]
