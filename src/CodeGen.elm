module CodeGen exposing
    ( dateSymbolParser
    , dateTimeTokenParser
    , formatSymbolParser
    , isSupportedKey
    , languageFile
    , mainLocaleFile
    , parseOptionsFromAvailableFormatKey
    , symbolListParserHelper
    , timeSymbolParser
    , withLiteralParser
    )

import Cldr.Format.Options exposing (DateOptions, DateTimeOptions, FractionalDigits(..), HourType(..), NameOption(..), NumberOption(..), NumberOrTextOption(..), TextOption(..))
import CodeGen.FormatSymbols
import DayPeriodsInfo exposing (DayPeriodsInfo)
import Dict
import Elm.CodeGen as Gen
import Elm.Pretty
import Internal.DayPeriodRule exposing (DayPeriodRule)
import Internal.FormatSymbols as Sym
import Internal.Options exposing (MinimalOptionSet(..), TimeOptions)
import Internal.Structures exposing (EraNames, MonthNames, Pattern3, Patterns, PeriodNames, WeekdayNames)
import LanguageInfo exposing (LanguageInfo, snakeIdentifier)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import String.Extra


localeAnn : Gen.TypeAnnotation
localeAnn =
    Gen.typed "Locale" []


langModule : String -> Gen.ModuleName
langModule lang =
    [ "Generated", String.Extra.toTitleCase lang ]


languageFile : String -> DayPeriodsInfo -> List LanguageInfo -> String
languageFile lang dayPeriods infos =
    Gen.file
        (Gen.normalModule (langModule lang)
            (List.map (snakeIdentifier >> Gen.funExpose) infos)
        )
        [ -- Gen.importStmt [ "Cldr", "Format", "Options" ] (Just [ "Opts" ]) Nothing
          Gen.importStmt [ "Dict" ] Nothing (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Dict" ]))
        , Gen.importStmt [ "Internal", "DayPeriodRule" ] Nothing Nothing
        , Gen.importStmt [ "Internal", "Locale" ] Nothing Nothing
        , Gen.importStmt [ "Internal", "Parse" ] Nothing Nothing

        -- , Gen.importStmt [ "Internal", "LanguageInfo" ] Nothing Nothing
        -- , Gen.importStmt [ "Internal", "FormatSymbols" ] (Just [ "Sym" ]) Nothing
        , Gen.importStmt [ "Internal", "Locale" ]
            Nothing
            (Just
                (Gen.exposeExplicit
                    [ Gen.openTypeExpose "DateTimeToken"
                    , Gen.openTypeExpose "LanguageId"
                    ]
                )
            )
        , Gen.importStmt [ "Tagged" ]
            Nothing
            (Just
                (Gen.exposeExplicit [ Gen.openTypeExpose "Tagged" ])
            )
        ]
        (dayPeriodRuleDeclaration lang dayPeriods
            :: List.map localeFileDeclaration infos
        )
        Nothing
        |> Elm.Pretty.pretty 80


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
        ([ Gen.importStmt [ "Internal", "Locale" ]
            Nothing
            (Just
                (Gen.exposeExplicit
                    [ Gen.openTypeExpose "DateTimeToken"
                    , Gen.openTypeExpose "LanguageId"
                    ]
                )
            )
         , Gen.importStmt [ "Tagged" ]
            Nothing
            (Just
                (Gen.exposeExplicit [ Gen.openTypeExpose "Tagged" ])
            )
         ]
            ++ List.map (Tuple.first >> generatedImport) groupedInfos
        )
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


generatedImport : String -> Gen.Import
generatedImport lang =
    Gen.importStmt [ "Generated", lang ] Nothing Nothing


mainLocaleDeclarations : List LanguageInfo -> List Gen.Declaration
mainLocaleDeclarations infos =
    [ localeTypeDeclaration
    , toUnicodeDeclaration
    , fromStringDeclaration
    , allLocalesDeclarationForMainLocale infos
    , basicLocalesDeclarationForMainLocale infos
    ]
        ++ List.map localeInMainFileDeclaration infos


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


localeInMainFileDeclaration : LanguageInfo -> Gen.Declaration
localeInMainFileDeclaration info =
    Gen.funDecl
        (Just (commentForLanguage info))
        (Just (Gen.typed "Locale" []))
        (snakeIdentifier info)
        []
        (Gen.fqVal (langModule info.language)
            (snakeIdentifier info)
        )


localeFileDeclaration : LanguageInfo -> Gen.Declaration
localeFileDeclaration info =
    Gen.valDecl
        (Just (commentForLanguage info))
        (Just (Gen.fqTyped [ "Internal", "Locale" ] "Locale" []))
        (snakeIdentifier info)
        (Gen.apply
            [ Gen.fqFun [ "Maybe" ] "withDefault"
            , Gen.fqVal [ "Internal", "Locale" ] "empty"
            , Gen.parens
                (Gen.apply
                    [ Gen.fqFun [ "Internal", "Parse" ] "parse"
                    , Gen.val "dayPeriods"
                    , generatedLangExpression info
                    ]
                )
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
        [ ( "language", Gen.string info.language )
        , ( "script", maybeExpr Gen.string info.script )
        , ( "territory", maybeExpr Gen.string info.territory )
        , ( "variant", maybeExpr Gen.string info.variant )
        , ( "periodNames", pattern3Expr periodNamesExpr info.periodNames )
        , ( "datePatterns", patternExpr Gen.string info.datePatterns )
        , ( "monthFormatNames", pattern3Expr monthNamesExpr info.monthFormatNames )
        , ( "monthStandaloneNames", pattern3Expr monthNamesExpr info.monthStandaloneNames )
        , ( "weekdayFormatNames", pattern3Expr weekdayNamesExpr info.weekdayFormatNames )
        , ( "weekdayStandaloneNames", pattern3Expr weekdayNamesExpr info.weekdayStandaloneNames )
        , ( "eraNames", pattern3Expr eraNamesExpr info.eraNames )
        , ( "timePatterns", patternExpr Gen.string info.timePatterns )
        , ( "dateTimePatterns", patternExpr Gen.string info.dateTimePatterns )
        , ( "availableFormats", Gen.list (List.map (\( k, v ) -> Gen.tuple [ Gen.string k, Gen.string v ]) info.availableFormats) )
        , ( "timeSkeletons", patternExpr Gen.string info.timeSkeletons )
        ]


dayPeriodRuleDeclaration : String -> DayPeriodsInfo -> Gen.Declaration
dayPeriodRuleDeclaration lang dayPeriods =
    let
        getSimpleLang : String -> String
        getSimpleLang =
            String.split "-" >> List.head >> Maybe.withDefault ""

        lowerLang =
            String.toLower lang
    in
    Gen.valDecl Nothing
        (Just (Gen.typed "Dict" [ Gen.typed "String" [], Gen.typed "List" [ Gen.fqTyped [ "Internal", "DayPeriodRule" ] "DayPeriodRule" [] ] ]))
        "dayPeriods"
        (Gen.apply
            [ Gen.fqFun [ "Dict" ] "fromList"
            , Dict.toList dayPeriods
                |> List.filter (Tuple.first >> getSimpleLang >> (==) lowerLang)
                |> List.map
                    (\( ruleLangTag, rules ) ->
                        Gen.tuple
                            [ Gen.string (String.Extra.underscored ruleLangTag)
                            , Gen.list (List.map dayPeriodRuleExpr rules)
                            ]
                    )
                |> Gen.list
            ]
        )


toRuleListName : String -> String
toRuleListName name =
    String.Extra.underscored name ++ "_dayPeriodRules"


dayPeriodRuleExpr : DayPeriodRule -> Gen.Expression
dayPeriodRuleExpr rule =
    case rule of
        Internal.DayPeriodRule.At at name ->
            Gen.apply
                [ Gen.fqFun [ "Internal", "DayPeriodRule" ] "At"
                , hourMinuteExpr at
                , Gen.string name
                ]

        Internal.DayPeriodRule.FromBefore from before name ->
            Gen.apply
                [ Gen.fqFun [ "Internal", "DayPeriodRule" ] "FromBefore"
                , hourMinuteExpr from
                , hourMinuteExpr before
                , Gen.string name
                ]


hourMinuteExpr : ( Int, Int ) -> Gen.Expression
hourMinuteExpr ( hour, minute ) =
    Gen.tuple [ Gen.int hour, Gen.int minute ]


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


pattern3Expr : (a -> Gen.Expression) -> Pattern3 a -> Gen.Expression
pattern3Expr toExpr pattern3 =
    Gen.record
        [ ( "abbreviated", toExpr pattern3.abbreviated )
        , ( "wide", toExpr pattern3.wide )
        , ( "narrow", toExpr pattern3.narrow )
        ]


eraNamesExpr : EraNames -> Gen.Expression
eraNamesExpr names =
    Gen.record
        [ ( "bc", Gen.string names.bc )
        , ( "ad", Gen.string names.ad )
        ]


periodNamesExpr : PeriodNames -> Gen.Expression
periodNamesExpr names =
    Gen.record
        [ ( "am", Gen.string names.am )
        , ( "pm", Gen.string names.pm )
        , ( "dayPeriods"
          , Gen.apply
                [ Gen.fqFun [ "Dict" ] "fromList"
                , Gen.list
                    (names.dayPeriods
                        |> Dict.toList
                        |> List.filter (Tuple.first >> (\k -> not (List.member k [ "am", "pm" ])))
                        |> List.map
                            (Tuple.mapBoth Gen.string Gen.string
                                >> (\( a, b ) -> Gen.tuple [ a, b ])
                            )
                    )
                ]
          )
        ]


dayPeriodRuleSetReferenceExpr : DayPeriodsInfo -> LanguageInfo -> Gen.Expression
dayPeriodRuleSetReferenceExpr dayPeriods info =
    let
        fullTag =
            [ Just info.language
            , info.script
            , info.territory
            , info.variant
            ]
                |> List.filterMap identity
                |> String.join "-"
    in
    if Dict.member fullTag dayPeriods then
        Gen.val (toRuleListName fullTag)

    else if Dict.member info.language dayPeriods then
        Gen.val (toRuleListName info.language)

    else
        Gen.list []


availableFormatListExpr : List ( String, String ) -> Gen.Expression
availableFormatListExpr =
    List.filter (Tuple.first >> isSupportedKey)
        >> List.map availableFormatExpr
        >> Gen.list


isSupportedKey : String -> Bool
isSupportedKey key =
    not (Set.member key unsupportedKeys)


unsupportedKeys : Set String
unsupportedKeys =
    Set.fromList
        [ "MMMMW-count-zero"
        , "MMMMW-count-one"
        , "MMMMW-count-two"
        , "MMMMW-count-few"
        , "MMMMW-count-many"
        , "MMMMW-count-other"
        , "MEd-alt-variant"
        , "Md-alt-variant"
        , "MMdd-alt-variant"
        , "yQ"
        , "yQQQ"
        , "yQQQQ"
        , "yMEd-alt-variant"
        , "yMd-alt-variant"
        , "yw-count-zero"
        , "yw-count-one"
        , "yw-count-two"
        , "yw-count-few"
        , "yw-count-many"
        , "yw-count-other"
        ]


availableFormatExpr : ( String, String ) -> Gen.Expression
availableFormatExpr ( key, pattern ) =
    case parseOptionsFromAvailableFormatKey key of
        Err _ ->
            Gen.val ("ERROR_CANNOT_PARSE_KEY:" ++ key)

        Ok opts ->
            availableFormatExprForOptions opts pattern


availableFormatExprForOptions : DateTimeOptions -> String -> Gen.Expression
availableFormatExprForOptions options pattern =
    case Internal.Options.shrinkOptions options of
        DateTimeSet dtOpts ->
            availableFormatDateTimeExpr dtOpts pattern

        DateSet dOpts ->
            availableFormatDateExpr dOpts pattern

        TimeSet tOpts ->
            availableFormatTimeExpr tOpts pattern

        EmptySet ->
            Gen.val "ERROR_EMPTY_OPTIONS"


availableFormatDateTimeExpr : DateTimeOptions -> String -> Gen.Expression
availableFormatDateTimeExpr opts pattern =
    Gen.apply
        [ Gen.fqFun [ "Internal", "Locale" ] "DateTimeAF"
        , Gen.record
            [ ( "options", dateTimeOptionExpr opts )
            , ( "formatSymbols", formatSymbolBestEffortExpr pattern )
            ]
        ]


availableFormatDateExpr : DateOptions -> String -> Gen.Expression
availableFormatDateExpr opts pattern =
    Gen.apply
        [ Gen.fqFun [ "Internal", "Locale" ] "DateAF"
        , Gen.record
            [ ( "options", dateOptionExpr opts )
            , ( "formatSymbols", dateSymbolBestEffortExpr pattern )
            ]
        ]


availableFormatTimeExpr : TimeOptions -> String -> Gen.Expression
availableFormatTimeExpr opts pattern =
    Gen.apply
        [ Gen.fqFun [ "Internal", "Locale" ] "TimeAF"
        , Gen.record
            [ ( "options", timeOptionExpr opts )
            , ( "formatSymbols", timeSymbolBestEffortExpr pattern )
            ]
        ]


hour12ByDefaultExpr : Patterns String -> Gen.Expression
hour12ByDefaultExpr patterns =
    let
        allPatterns =
            [ patterns.short, patterns.medium, patterns.long, patterns.full ]

        contains24Hour =
            List.any (String.contains "H") allPatterns

        contains12Hour =
            List.any (String.contains "h") allPatterns
    in
    case ( contains12Hour, contains24Hour ) of
        ( True, True ) ->
            Gen.val "ERROR_BOTH_HOUR_TYPES"

        ( True, False ) ->
            Gen.val "True"

        ( False, True ) ->
            Gen.val "False"

        ( False, False ) ->
            Gen.val "ERROR_NEITHER_HOUR_TYPE"


listParserHelper : Parser a -> (a -> Gen.Expression) -> String -> Gen.Expression
listParserHelper innerParser toExpr pattern =
    Parser.run (symbolListParserHelper innerParser) pattern
        |> Result.map (List.map toExpr >> Gen.list)
        |> Result.withDefault (Gen.string ("ERROR: " ++ pattern))


formatSymbolBestEffortExpr : String -> Gen.Expression
formatSymbolBestEffortExpr =
    listParserHelper (withLiteralParser formatSymbolParser)
        CodeGen.FormatSymbols.formatWithLiteral


dateSymbolBestEffortExpr : String -> Gen.Expression
dateSymbolBestEffortExpr =
    listParserHelper (withLiteralParser dateSymbolParser)
        CodeGen.FormatSymbols.dateWithLiteral


timeSymbolBestEffortExpr : String -> Gen.Expression
timeSymbolBestEffortExpr =
    listParserHelper (withLiteralParser timeSymbolParser)
        CodeGen.FormatSymbols.timeWithLiteral


symbolListParserHelper : Parser a -> Parser (List a)
symbolListParserHelper innerParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.succeed ()
        , item = innerParser
        , trailing = Parser.Optional
        }
        |. Parser.end


parseWord : String -> a -> Parser a
parseWord word item =
    Parser.map (\_ -> item) (Parser.symbol word)


parseTextWidth5 : String -> (Sym.TextWidth -> a) -> Parser a
parseTextWidth5 baseCharacter toItem =
    Parser.oneOf
        [ parseWord (String.repeat 5 baseCharacter) (toItem Sym.Narrow)
        , parseWord (String.repeat 4 baseCharacter) (toItem Sym.Wide)
        , parseWord (String.repeat 3 baseCharacter) (toItem Sym.Abbreviated)
        , parseWord (String.repeat 2 baseCharacter) (toItem Sym.Abbreviated)
        , parseWord baseCharacter (toItem Sym.Abbreviated)
        ]


parseNumberWidth2 : String -> (Sym.NumberWidth -> a) -> Parser a
parseNumberWidth2 baseCharacter toItem =
    Parser.oneOf
        [ parseWord (String.repeat 2 baseCharacter) (toItem Sym.TwoDigit)
        , parseWord baseCharacter (toItem Sym.MinimumDigits)
        ]


parseWidth5 : String -> (Sym.Width -> a) -> Parser a
parseWidth5 baseCharacter toItem =
    Parser.oneOf
        [ parseWord (String.repeat 5 baseCharacter) (toItem (Sym.Text Sym.Narrow))
        , parseWord (String.repeat 4 baseCharacter) (toItem (Sym.Text Sym.Wide))
        , parseWord (String.repeat 3 baseCharacter) (toItem (Sym.Text Sym.Abbreviated))
        , parseWord (String.repeat 2 baseCharacter) (toItem (Sym.Number Sym.TwoDigit))
        , parseWord baseCharacter (toItem (Sym.Number Sym.MinimumDigits))
        ]


withLiteralParser : Parser a -> Parser (Sym.WithLiteral a)
withLiteralParser innerParser =
    Parser.oneOf
        [ Parser.map Sym.Symbol innerParser
        , Parser.variable
            { start = \c -> not (Char.isAlpha c) && c /= '\''
            , inner = \c -> not (Char.isAlpha c) && c /= '\''
            , reserved = Set.empty
            }
            |> Parser.map Sym.Literal
        , Parser.succeed Sym.Literal
            |. Parser.symbol "'"
            |= Parser.getChompedString (Parser.chompUntil "'")
            |. Parser.symbol "'"
        ]


formatSymbolParser : Parser Sym.FormatSymbol
formatSymbolParser =
    Parser.oneOf
        [ Parser.map Sym.Date dateSymbolParser
        , Parser.map Sym.Time timeSymbolParser
        ]


dateSymbolParser : Parser Sym.DateSymbol
dateSymbolParser =
    Parser.oneOf
        [ parseTextWidth5 "G" Sym.Era
        , parseNumberWidth2 "y" Sym.Year
        , parseWidth5 "M" Sym.Month
        , parseWidth5 "L" Sym.MonthStandalone
        , parseTextWidth5 "E" Sym.Weekday
        , parseWord "ccccc" (Sym.WeekdayStandalone Sym.Narrow)
        , parseWord "cccc" (Sym.WeekdayStandalone Sym.Wide)
        , parseWord "ccc" (Sym.WeekdayStandalone Sym.Abbreviated)
        , parseNumberWidth2 "d" Sym.Day
        ]


timeSymbolParser : Parser Sym.TimeSymbol
timeSymbolParser =
    Parser.oneOf
        [ parseTextWidth5 "a" Sym.Period
        , parseTextWidth5 "B" Sym.FlexibleDayPeriod
        , parseNumberWidth2 "h" Sym.Hour12From1
        , parseNumberWidth2 "H" Sym.Hour24From0
        , parseNumberWidth2 "K" Sym.Hour12From0
        , parseNumberWidth2 "k" Sym.Hour24From1
        , parseNumberWidth2 "m" Sym.Minute
        , parseNumberWidth2 "s" Sym.Second
        , parseWord "SSS" (Sym.FractionalSeconds 3)
        , parseWord "SS" (Sym.FractionalSeconds 2)
        , parseWord "S" (Sym.FractionalSeconds 1)
        , parseWord "zzzz" (Sym.ZoneNonLocationFormat Sym.Long)
        , parseWord "zzz" (Sym.ZoneNonLocationFormat Sym.Short)
        , parseWord "zz" (Sym.ZoneNonLocationFormat Sym.Short)
        , parseWord "z" (Sym.ZoneNonLocationFormat Sym.Short)
        , parseWord "Z" Sym.ZoneIso8601Basic
        , parseWord "OOOO" (Sym.ZoneGmtFormat Sym.Long)
        , parseWord "O" (Sym.ZoneGmtFormat Sym.Short)
        , parseWord "vvvv" (Sym.ZoneGenericNonLocationFormat Sym.Long)
        , parseWord "v" (Sym.ZoneGenericNonLocationFormat Sym.Short)
        ]


dateTimeTokenBestEffortExpr : String -> Gen.Expression
dateTimeTokenBestEffortExpr =
    Parser.run (symbolListParserHelper dateTimeTokenParser)
        >> Result.map Gen.list
        >> Result.withDefault (Gen.val "ERROR")


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



-- Available Format Key parsing


parseOptionsFromAvailableFormatKey : String -> Result (List Parser.DeadEnd) DateTimeOptions
parseOptionsFromAvailableFormatKey =
    Parser.run availableFormatKeyOptionsParser


availableFormatKeyOptionsParser : Parser DateTimeOptions
availableFormatKeyOptionsParser =
    Parser.succeed buildKeyOptions
        |= parseEra
        |= parseYear
        |= parseMonth
        |= parseWeekday
        |= parseDay
        |= parsePeriod
        |= parseDayPeriod
        |= parseHour
        |= parseMinute
        |= parseSecond
        |= parseZone
        |. Parser.end


buildKeyOptions : Maybe TextOption -> Maybe NumberOption -> Maybe NumberOrTextOption -> Maybe TextOption -> Maybe NumberOption -> Maybe TextOption -> Maybe TextOption -> Maybe ( NumberOption, HourType ) -> Maybe NumberOption -> Maybe NumberOption -> Maybe NameOption -> DateTimeOptions
buildKeyOptions era year month weekday day period dayPeriod hourAndType minute second zone =
    { era = era
    , year = year
    , month = month
    , day = day
    , weekday = weekday
    , period = period
    , dayPeriod = dayPeriod
    , hour = Maybe.map Tuple.first hourAndType
    , minute = minute
    , second = second
    , fractionalSecondDigits = Nothing
    , zone = zone
    , hour12 = Maybe.map Tuple.second hourAndType
    }


parseOptionalWords : List ( String, a ) -> Parser (Maybe a)
parseOptionalWords list =
    Parser.oneOf
        (List.map
            (\( word, item ) ->
                parseWord word (Just item)
            )
            list
            ++ [ Parser.succeed Nothing ]
        )


parseEra : Parser (Maybe TextOption)
parseEra =
    parseOptionalWords
        [ ( "GGGGG", Narrow )
        , ( "GGGG", Long )
        , ( "G", Short )
        ]


parseYear : Parser (Maybe NumberOption)
parseYear =
    parseOptionalWords
        [ ( "yy", TwoDigit )
        , ( "y", Numeric )
        ]


parseMonth : Parser (Maybe NumberOrTextOption)
parseMonth =
    parseOptionalWords
        [ ( "MMMMM", Text Narrow )
        , ( "MMMM", Text Long )
        , ( "MMM", Text Short )
        , ( "MM", Number TwoDigit )
        , ( "M", Number Numeric )
        ]


parseDay : Parser (Maybe NumberOption)
parseDay =
    parseOptionalWords
        [ ( "dd", TwoDigit )
        , ( "d", Numeric )
        ]


parseWeekday : Parser (Maybe TextOption)
parseWeekday =
    parseOptionalWords
        [ ( "EEEEE", Narrow )
        , ( "EEEE", Long )
        , ( "E", Short )
        , ( "cccc", Long )
        ]


parsePeriod : Parser (Maybe TextOption)
parsePeriod =
    parseOptionalWords
        [ ( "aaaaa", Narrow )
        , ( "aaaa", Long )
        , ( "a", Short )
        ]


parseDayPeriod : Parser (Maybe TextOption)
parseDayPeriod =
    parseOptionalWords
        [ ( "BBBBB", Narrow )
        , ( "BBBB", Long )
        , ( "BBB", Short )
        , ( "BB", Short )
        , ( "B", Short )
        ]


parseHour : Parser (Maybe ( NumberOption, HourType ))
parseHour =
    parseOptionalWords
        [ ( "hh", ( TwoDigit, Hour12 ) )
        , ( "h", ( Numeric, Hour12 ) )
        , ( "HH", ( TwoDigit, Hour24 ) )
        , ( "H", ( Numeric, Hour24 ) )
        ]


parseMinute : Parser (Maybe NumberOption)
parseMinute =
    parseOptionalWords
        [ ( "mm", TwoDigit )
        , ( "m", Numeric )
        ]


parseSecond : Parser (Maybe NumberOption)
parseSecond =
    parseOptionalWords
        [ ( "ss", TwoDigit )
        , ( "s", Numeric )
        ]


parseZone : Parser (Maybe NameOption)
parseZone =
    parseOptionalWords
        [ ( "zzzz", LongName )
        , ( "z", ShortName )
        , ( "vvvv", LongName )
        , ( "v", ShortName )
        , ( "Z", ShortName )
        ]



-- Code generation for options records


dateTimeOptionExpr : DateTimeOptions -> Gen.Expression
dateTimeOptionExpr opts =
    Gen.record
        [ ( "era", maybeExpr textOptionExpr opts.era )
        , ( "year", maybeExpr numberOptionExpr opts.year )
        , ( "month", maybeExpr numberOrTextOptionExpr opts.month )
        , ( "day", maybeExpr numberOptionExpr opts.day )
        , ( "weekday", maybeExpr textOptionExpr opts.weekday )
        , ( "period", maybeExpr textOptionExpr opts.period )
        , ( "dayPeriod", maybeExpr textOptionExpr opts.dayPeriod )
        , ( "hour", maybeExpr numberOptionExpr opts.hour )
        , ( "minute", maybeExpr numberOptionExpr opts.minute )
        , ( "second", maybeExpr numberOptionExpr opts.second )
        , ( "fractionalSecondDigits", maybeExpr fractionalDigitsExpr opts.fractionalSecondDigits )
        , ( "zone", maybeExpr nameOptionExpr opts.zone )
        , ( "hour12", maybeExpr hourTypeExpr opts.hour12 )
        ]


dateOptionExpr : DateOptions -> Gen.Expression
dateOptionExpr opts =
    Gen.record
        [ ( "era", maybeExpr textOptionExpr opts.era )
        , ( "year", maybeExpr numberOptionExpr opts.year )
        , ( "month", maybeExpr numberOrTextOptionExpr opts.month )
        , ( "day", maybeExpr numberOptionExpr opts.day )
        , ( "weekday", maybeExpr textOptionExpr opts.weekday )
        ]


timeOptionExpr : TimeOptions -> Gen.Expression
timeOptionExpr opts =
    Gen.record
        [ ( "period", maybeExpr textOptionExpr opts.period )
        , ( "dayPeriod", maybeExpr textOptionExpr opts.dayPeriod )
        , ( "hour", maybeExpr numberOptionExpr opts.hour )
        , ( "minute", maybeExpr numberOptionExpr opts.minute )
        , ( "second", maybeExpr numberOptionExpr opts.second )
        , ( "fractionalSecondDigits", maybeExpr fractionalDigitsExpr opts.fractionalSecondDigits )
        , ( "zone", maybeExpr nameOptionExpr opts.zone )
        , ( "hour12", maybeExpr hourTypeExpr opts.hour12 )
        ]


maybeExpr : (a -> Gen.Expression) -> Maybe a -> Gen.Expression
maybeExpr toExpr maybeItem =
    case maybeItem of
        Just item ->
            Gen.apply [ Gen.fun "Just", toExpr item ]

        Nothing ->
            Gen.val "Nothing"


textOptionExpr : TextOption -> Gen.Expression
textOptionExpr opt =
    case opt of
        Narrow ->
            Gen.fqVal [ "Opts" ] "Narrow"

        Short ->
            Gen.fqVal [ "Opts" ] "Short"

        Long ->
            Gen.fqVal [ "Opts" ] "Long"


numberOptionExpr : NumberOption -> Gen.Expression
numberOptionExpr opt =
    case opt of
        Numeric ->
            Gen.fqVal [ "Opts" ] "Numeric"

        TwoDigit ->
            Gen.fqVal [ "Opts" ] "TwoDigit"


numberOrTextOptionExpr : NumberOrTextOption -> Gen.Expression
numberOrTextOptionExpr opt =
    case opt of
        Text textOpt ->
            Gen.parens (Gen.apply [ Gen.fqFun [ "Opts" ] "Text", textOptionExpr textOpt ])

        Number numOpt ->
            Gen.parens (Gen.apply [ Gen.fqFun [ "Opts" ] "Number", numberOptionExpr numOpt ])


fractionalDigitsExpr : FractionalDigits -> Gen.Expression
fractionalDigitsExpr digits =
    case digits of
        One ->
            Gen.fqVal [ "Opts" ] "One"

        Two ->
            Gen.fqVal [ "Opts" ] "Two"

        Three ->
            Gen.fqVal [ "Opts" ] "Three"


nameOptionExpr : NameOption -> Gen.Expression
nameOptionExpr opt =
    case opt of
        ShortName ->
            Gen.fqVal [ "Opts" ] "ShortName"

        LongName ->
            Gen.fqVal [ "Opts" ] "LongName"


hourTypeExpr : HourType -> Gen.Expression
hourTypeExpr hourType =
    case hourType of
        Hour12 ->
            Gen.fqVal [ "Opts" ] "Hour12"

        Hour24 ->
            Gen.fqVal [ "Opts" ] "Hour24"
