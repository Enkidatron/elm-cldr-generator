module CodeGen exposing
    ( languageFile
    , mainLocaleFile
    )

import Cldr.Format.Options exposing (FractionalDigits(..), HourType(..), NameOption(..), NumberOption(..), NumberOrTextOption(..), TextOption(..))
import DayPeriodsInfo exposing (DayPeriodsInfo)
import Dict
import Elm
import Elm.Annotation
import Elm.Op
import Gen.Dict
import Gen.Internal.DayPeriodRule
import Gen.Internal.LanguageInfo
import Gen.Internal.Locale
import Gen.Internal.Parse
import Gen.Internal.Structures
import Gen.Maybe
import Internal.DayPeriodRule exposing (DayPeriodRule)
import Internal.LanguageInfo exposing (LanguageInfo)
import Internal.LanguageInfo.Encode
import Internal.Options exposing (MinimalOptionSet(..))
import Internal.Structures exposing (EraNames, MonthNames, Pattern3, Patterns, PeriodNames, WeekdayNames)
import LanguageInfo.Extra exposing (snakeIdentifier)
import List.Extra
import String.Extra


localeAnnotation : Elm.Annotation.Annotation
localeAnnotation =
    Elm.Annotation.named [] "Locale"


languageFile : String -> DayPeriodsInfo -> List LanguageInfo -> String
languageFile lang dayPeriods infos =
    Elm.fileWith [ "Generated", String.Extra.toTitleCase lang ]
        { docs = List.map (sortInGroup >> Elm.docs)
        , aliases = []
        }
        (dayPeriodRuleDeclaration lang dayPeriods
            :: List.map localeFileDeclaration infos
        )
        |> .contents


sortInGroup : { a | members : List comparable } -> { a | members : List comparable }
sortInGroup record =
    { record | members = List.sort record.members }


mainLocaleFile : List ( String, List LanguageInfo ) -> String
mainLocaleFile groupedInfos =
    let
        infos =
            List.concatMap Tuple.second groupedInfos
    in
    Elm.fileWith [ "Cldr", "Locale" ]
        { docs =
            always (mainLocaleFileComment groupedInfos)
        , aliases = []
        }
        (mainLocaleDeclarations infos)
        |> .contents


mainLocaleFileComment : List ( String, List LanguageInfo ) -> List String
mainLocaleFileComment groupedInfos =
    [ "# Locale"
    , docTags [ "Locale" ]
    , "## Create"
    , docTags [ "fromString" ]
    , docTags [ "allLocales", "basicLocales" ]
    , "## Convert"
    , docTags [ "toUnicode" ]
    , "## Locales by language"
    ]
        ++ List.concatMap docForLangGroup groupedInfos


docTags : List String -> String
docTags words =
    "@docs " ++ String.join ", " words


docForLangGroup : ( String, List LanguageInfo ) -> List String
docForLangGroup ( langTag, infos ) =
    ("### " ++ langTag)
        :: List.map (\infoGroup -> docTags (List.map snakeIdentifier infoGroup)) (List.Extra.greedyGroupsOf 16 infos)


mainLocaleDeclarations : List LanguageInfo -> List Elm.Declaration
mainLocaleDeclarations infos =
    [ localeTypeDeclaration
    , toUnicodeDeclaration
    , fromStringDeclaration
    , allLocalesDeclarationForMainLocale infos
    , basicLocalesDeclarationForMainLocale infos
    ]
        ++ List.map localeInMainFileDeclaration infos


localeTypeDeclaration : Elm.Declaration
localeTypeDeclaration =
    Elm.alias "Locale"
        Gen.Internal.Locale.annotation_.locale
        |> Elm.withDocumentation localeTypeDocComment
        |> exposeWithGroup "Locale"


exposeWithGroup : String -> Elm.Declaration -> Elm.Declaration
exposeWithGroup group =
    Elm.exposeWith { exposeConstructor = True, group = Just group }


localeTypeDocComment : String
localeTypeDocComment =
    String.join "\n"
        [ "Represents the locale to use for formatting."
        , ""
        , "You can parse a value from JS like `navigator.language` or use hardcoded locales."
        , ""
        ]


toUnicodeDeclaration : Elm.Declaration
toUnicodeDeclaration =
    Elm.fn ( "locale", Nothing )
        (\locale ->
            Gen.Internal.Locale.toUnicode locale
        )
        |> Elm.declaration "toUnicode"
        |> Elm.withDocumentation toUnicodeDocComment
        |> exposeWithGroup "Convert"


toUnicodeDocComment : String
toUnicodeDocComment =
    String.join "\n"
        [ "Get the [Unicode](https://unicode.org/reports/tr35/#Identifiers) representation of a locale."
        , ""
        , "    toUnicode en_GB"
        , "    --> \"en-GB\""
        ]


fromStringDeclaration : Elm.Declaration
fromStringDeclaration =
    Elm.fn2 ( "candidateLocales", Nothing )
        ( "localeString", Nothing )
        (\candidateLocales localeString ->
            Elm.apply Gen.Internal.Locale.values_.languageIdFromString [ localeString ]
                |> Elm.Op.pipe (Elm.apply Gen.Maybe.values_.andThen [ Elm.apply Gen.Internal.Locale.values_.matchNearestLocale [ candidateLocales ] ])
        )
        |> Elm.declaration "fromString"
        |> Elm.withDocumentation fromStringDocComment
        |> exposeWithGroup "Create"


fromStringDocComment : String
fromStringDocComment =
    String.join "\n"
        [ "Parse a `Locale` from a Unicode or BCP47 identifier."
        , ""
        , "    fromString basicLocales \"en\""
        , "    --> Just en"
        , ""
        , "    fromString allLocales \"en-GB\""
        , "    --> Just en_GB"
        ]


allLocalesDeclarationForMainLocale : List LanguageInfo -> Elm.Declaration
allLocalesDeclarationForMainLocale infos =
    Elm.list (List.map (snakeIdentifier >> Elm.val) infos)
        |> Elm.withType (Elm.Annotation.list localeAnnotation)
        |> Elm.declaration "allLocales"
        |> Elm.withDocumentation "A list of every locale listed in the JSON version of the CLDR."
        |> exposeWithGroup "Create"


basicLocalesDeclarationForMainLocale : List LanguageInfo -> Elm.Declaration
basicLocalesDeclarationForMainLocale infos =
    Elm.list (List.filterMap (basicLanguage >> Maybe.map (snakeIdentifier >> Elm.val)) infos)
        |> Elm.withType (Elm.Annotation.list localeAnnotation)
        |> Elm.declaration "basicLocales"
        |> Elm.withDocumentation basicLocalesDocComment
        |> exposeWithGroup "Create"


basicLocalesDocComment : String
basicLocalesDocComment =
    [ """A list of every "basic" locale listed in the JSON version of the CLDR."""
    , """A "basic" locale is a locale without a region, script, or variant subtag, such as `en` or `ru`."""
    ]
        |> String.join "\n\n"


localeInMainFileDeclaration : LanguageInfo -> Elm.Declaration
localeInMainFileDeclaration info =
    Elm.value
        { importFrom = [ "Generated", String.Extra.toTitleCase info.language ]
        , name = snakeIdentifier info
        , annotation = Just localeAnnotation
        }
        |> Elm.declaration (snakeIdentifier info)
        |> Elm.withDocumentation (commentForLanguage info)
        |> Elm.expose


localeFileDeclaration : LanguageInfo -> Elm.Declaration
localeFileDeclaration info =
    Gen.Maybe.withDefault
        Gen.Internal.Locale.empty
        (Gen.Internal.Parse.parse
            (Elm.val "dayPeriods")
            (Internal.LanguageInfo.Encode.encode (Internal.LanguageInfo.compact info))
        )
        |> Elm.withType Gen.Internal.Locale.annotation_.locale
        |> Elm.declaration (snakeIdentifier info)
        |> Elm.withDocumentation (commentForLanguage info)
        |> Elm.expose


commentForLanguage : LanguageInfo -> String
commentForLanguage info =
    String.concat
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


basicLanguage : LanguageInfo -> Maybe LanguageInfo
basicLanguage info =
    case ( info.script, info.territory, info.variant ) of
        ( Nothing, Nothing, Nothing ) ->
            Just info

        _ ->
            Nothing


generatedLangExpression : LanguageInfo -> Elm.Expression
generatedLangExpression info =
    Gen.Internal.LanguageInfo.expand (generateCompactExpression (Internal.LanguageInfo.compact info))


generateCompactExpression : Internal.LanguageInfo.Compact -> Elm.Expression
generateCompactExpression info =
    Gen.Internal.LanguageInfo.make_.compact
        { language = Elm.string info.language
        , script = maybeExpr Elm.string info.script
        , territory = maybeExpr Elm.string info.territory
        , variant = maybeExpr Elm.string info.variant
        , periodNames = pattern3Expr periodNamesExpr info.periodNames
        , datePatterns = patternExpr Elm.string info.datePatterns
        , monthFormatNames = pattern3Expr monthNamesExpr info.monthFormatNames
        , monthStandaloneNames = maybeExpr (pattern3Expr monthNamesExpr) info.monthStandaloneNames
        , weekdayFormatNames = pattern3Expr weekdayNamesExpr info.weekdayFormatNames
        , weekdayStandaloneNames = maybeExpr (pattern3Expr weekdayNamesExpr) info.weekdayStandaloneNames
        , eraNames = pattern3Expr eraNamesExpr info.eraNames
        , timePatterns = patternExpr Elm.string info.timePatterns
        , dateTimePatterns = patternExpr Elm.string info.dateTimePatterns
        , availableFormats = Elm.list (List.map (\( k, v ) -> Elm.tuple (Elm.string k) (Elm.string v)) info.availableFormats)
        , timeSkeletons = patternExpr Elm.string info.timeSkeletons
        }


dayPeriodRuleDeclaration : String -> DayPeriodsInfo -> Elm.Declaration
dayPeriodRuleDeclaration lang dayPeriods =
    let
        getSimpleLang : String -> String
        getSimpleLang =
            String.split "-" >> List.head >> Maybe.withDefault ""

        lowerLang =
            String.toLower lang
    in
    Gen.Dict.fromList
        (Dict.toList dayPeriods
            |> List.filter (Tuple.first >> getSimpleLang >> (==) lowerLang)
            |> List.map
                (\( ruleLangTag, rules ) ->
                    Elm.tuple
                        (Elm.string (String.Extra.underscored ruleLangTag))
                        (Elm.list (List.map dayPeriodRuleExpr rules))
                )
        )
        |> Elm.withType (Gen.Dict.annotation_.dict Elm.Annotation.string (Elm.Annotation.list Gen.Internal.DayPeriodRule.annotation_.dayPeriodRule))
        |> Elm.declaration "dayPeriods"


dayPeriodRuleExpr : DayPeriodRule -> Elm.Expression
dayPeriodRuleExpr rule =
    case rule of
        Internal.DayPeriodRule.At at name ->
            Gen.Internal.DayPeriodRule.make_.at
                (hourMinuteExpr at)
                (Elm.string name)

        Internal.DayPeriodRule.FromBefore from before name ->
            Gen.Internal.DayPeriodRule.make_.fromBefore
                (hourMinuteExpr from)
                (hourMinuteExpr before)
                (Elm.string name)


hourMinuteExpr : ( Int, Int ) -> Elm.Expression
hourMinuteExpr ( hour, minute ) =
    Elm.tuple (Elm.int hour) (Elm.int minute)


patternExpr : (a -> Elm.Expression) -> Patterns a -> Elm.Expression
patternExpr f pattern =
    Internal.Structures.mapPattern f pattern
        |> Gen.Internal.Structures.make_.patterns


monthNamesExpr : MonthNames -> Elm.Expression
monthNamesExpr names =
    Gen.Internal.Structures.make_.monthNames
        { jan = Elm.string names.jan
        , feb = Elm.string names.feb
        , mar = Elm.string names.mar
        , apr = Elm.string names.apr
        , may = Elm.string names.may
        , jun = Elm.string names.jun
        , jul = Elm.string names.jul
        , aug = Elm.string names.aug
        , sep = Elm.string names.sep
        , oct = Elm.string names.oct
        , nov = Elm.string names.nov
        , dec = Elm.string names.dec
        }


weekdayNamesExpr : WeekdayNames -> Elm.Expression
weekdayNamesExpr names =
    Gen.Internal.Structures.make_.weekdayNames
        { sun = Elm.string names.sun
        , mon = Elm.string names.mon
        , tue = Elm.string names.tue
        , wed = Elm.string names.wed
        , thu = Elm.string names.thu
        , fri = Elm.string names.fri
        , sat = Elm.string names.sat
        }


pattern3Expr : (a -> Elm.Expression) -> Pattern3 a -> Elm.Expression
pattern3Expr f pattern3 =
    Internal.Structures.mapPattern3 f pattern3
        |> Gen.Internal.Structures.make_.pattern3


eraNamesExpr : EraNames -> Elm.Expression
eraNamesExpr names =
    Gen.Internal.Structures.make_.eraNames
        { bc = Elm.string names.bc
        , ad = Elm.string names.ad
        }


periodNamesExpr : PeriodNames -> Elm.Expression
periodNamesExpr names =
    Gen.Internal.Structures.make_.periodNames
        { am = Elm.string names.am
        , pm = Elm.string names.pm
        , dayPeriods =
            Gen.Dict.fromList
                (Dict.toList names.dayPeriods
                    |> List.filter (Tuple.first >> (\k -> not (List.member k [ "am", "pm" ])))
                    |> List.map
                        (\( a, b ) -> Elm.tuple (Elm.string a) (Elm.string b))
                )
        }


maybeExpr : (a -> Elm.Expression) -> Maybe a -> Elm.Expression
maybeExpr toExpr maybeItem =
    case maybeItem of
        Just item ->
            Gen.Maybe.make_.just (toExpr item)

        Nothing ->
            Gen.Maybe.make_.nothing
