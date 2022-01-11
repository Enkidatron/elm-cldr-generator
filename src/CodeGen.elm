module CodeGen exposing (dateTimeFormatTokenListParser, localeFile, mainGeneratedFile)

import Elm.CodeGen as Gen
import Elm.Pretty
import Internal.Structures exposing (EraNames, MonthNames, Patterns, WeekdayNames)
import LanguageInfo exposing (LanguageInfo, snakeIdentifier)
import Parser exposing ((|.), (|=), Parser)
import Set


localeFile : String -> List LanguageInfo -> String
localeFile langName infos =
    Gen.file
        (Gen.normalModule [ "Cldr", "Locale", langName ]
            (List.map (snakeIdentifier >> Gen.funExpose) infos)
        )
        [ Gen.importStmt [ "Internal", "Locale" ]
            Nothing
            (Just
                (Gen.exposeExplicit
                    [ Gen.openTypeExpose "Locale"
                    ]
                )
            )
        , Gen.importStmt [ "Internal", "Generated" ]
            Nothing
            Nothing
        ]
        (List.map localeFileDeclaration infos)
        (Just (localeFileComment langName infos))
        |> Elm.Pretty.pretty 80


localeFileComment : String -> List LanguageInfo -> Gen.Comment Gen.FileComment
localeFileComment langName infos =
    Gen.emptyFileComment
        |> Gen.markdown ("# Locales for the " ++ langName ++ " language")
        |> Gen.docTags (List.map snakeIdentifier infos)


localeFileDeclaration : LanguageInfo -> Gen.Declaration
localeFileDeclaration info =
    Gen.funDecl Nothing
        (Just (Gen.typed "Locale" []))
        (snakeIdentifier info)
        []
        (Gen.apply
            [ Gen.fun "Locale"
            , Gen.fqVal [ "Internal", "Generated" ] (snakeIdentifier info)
            ]
        )


mainGeneratedFile : List LanguageInfo -> String
mainGeneratedFile infos =
    Gen.file
        (Gen.normalModule [ "Internal", "Generated" ]
            (infos
                |> List.map snakeIdentifier
                |> (::) "allLocales"
                |> List.map Gen.funExpose
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
        (allLocalesDeclaration infos
            :: List.map generatedLangDeclaration infos
        )
        Nothing
        |> Elm.Pretty.pretty 80


allLocalesDeclaration : List LanguageInfo -> Gen.Declaration
allLocalesDeclaration infos =
    Gen.funDecl Nothing
        (Just (Gen.typed "List" [ Gen.typed "Internal" [] ]))
        "allLocales"
        []
        (Gen.list (List.map (snakeIdentifier >> Gen.val) infos))


generatedLangDeclaration : LanguageInfo -> Gen.Declaration
generatedLangDeclaration info =
    Gen.funDecl Nothing
        (Just (Gen.typed "Internal" []))
        (snakeIdentifier info)
        []
        (generatedLangExpression info)


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
