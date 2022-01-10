module LanguageInfo exposing (LanguageInfo, decoder, snakeIdentifier)

import Internal.Structures exposing (AmPmNames, EraNames, MonthNames, Patterns, WeekdayNames)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDPipe
import Set exposing (Set)


type alias LanguageInfo =
    { language : String
    , script : Maybe String
    , territory : Maybe String
    , variant : Maybe String
    , amPmNames : AmPmNames
    , datePatterns : Patterns String
    , monthNames : MonthNames
    , monthNamesShort : MonthNames
    , weekdayNames : WeekdayNames
    , weekdayNamesShort : WeekdayNames
    , eraNames : EraNames
    , timePatterns : Patterns String
    , dateTimePatterns : Patterns String
    }


decoder : String -> Decoder LanguageInfo
decoder langCode =
    JD.at [ "main", langCode ] languageInfoDecoder


languageInfoDecoder : Decoder LanguageInfo
languageInfoDecoder =
    JD.succeed LanguageInfo
        |> JDPipe.requiredAt [ "identity", "language" ] JD.string
        |> JDPipe.optionalAt [ "identity", "script" ] (JD.nullable JD.string) Nothing
        |> JDPipe.optionalAt [ "identity", "territory" ] (JD.nullable JD.string) Nothing
        |> JDPipe.optionalAt [ "identity", "variant" ] (JD.nullable JD.string) Nothing
        |> JDPipe.custom amPmNamesDecoder
        |> JDPipe.custom datePatternsDecoder
        |> JDPipe.custom monthNamesDecoder
        |> JDPipe.custom monthNamesShortDecoder
        |> JDPipe.custom weekdayNamesDecoder
        |> JDPipe.custom weekdayNamesShortDecoder
        |> JDPipe.custom eraNamesDecoder
        |> JDPipe.custom timePatternsDecoder
        |> JDPipe.custom dateTimePatternsDecoder


amPmNamesDecoder : Decoder AmPmNames
amPmNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "dayPeriods", "format", "abbreviated" ]
        (JD.succeed AmPmNames
            |> JDPipe.required "am" JD.string
            |> JDPipe.required "pm" JD.string
        )


datePatternsDecoder : Decoder (Patterns String)
datePatternsDecoder =
    JD.at [ "dates", "calendars", "gregorian", "dateFormats" ]
        (patternDecoder JD.string)


timePatternsDecoder : Decoder (Patterns String)
timePatternsDecoder =
    JD.at [ "dates", "calendars", "gregorian", "timeFormats" ]
        (patternDecoder JD.string)


dateTimePatternsDecoder : Decoder (Patterns String)
dateTimePatternsDecoder =
    JD.at [ "dates", "calendars", "gregorian", "dateTimeFormats" ]
        (patternDecoder JD.string)


monthNamesDecoder : Decoder MonthNames
monthNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "months", "format", "wide" ]
        genericMonthNamesDecoder


monthNamesShortDecoder : Decoder MonthNames
monthNamesShortDecoder =
    JD.at [ "dates", "calendars", "gregorian", "months", "format", "abbreviated" ]
        genericMonthNamesDecoder


weekdayNamesDecoder : Decoder WeekdayNames
weekdayNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "days", "format", "wide" ]
        genericWeekdayNamesDecoder


weekdayNamesShortDecoder : Decoder WeekdayNames
weekdayNamesShortDecoder =
    JD.at [ "dates", "calendars", "gregorian", "days", "format", "abbreviated" ]
        genericWeekdayNamesDecoder


eraNamesDecoder : Decoder EraNames
eraNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "eras", "eraAbbr" ]
        genericEraNamesDecoder


patternDecoder : Decoder a -> Decoder (Patterns a)
patternDecoder innerDecoder =
    JD.succeed Patterns
        |> JDPipe.required "short" innerDecoder
        |> JDPipe.required "medium" innerDecoder
        |> JDPipe.required "long" innerDecoder
        |> JDPipe.required "full" innerDecoder


genericMonthNamesDecoder : Decoder MonthNames
genericMonthNamesDecoder =
    JD.succeed MonthNames
        |> JDPipe.required "1" JD.string
        |> JDPipe.required "2" JD.string
        |> JDPipe.required "3" JD.string
        |> JDPipe.required "4" JD.string
        |> JDPipe.required "5" JD.string
        |> JDPipe.required "6" JD.string
        |> JDPipe.required "7" JD.string
        |> JDPipe.required "8" JD.string
        |> JDPipe.required "9" JD.string
        |> JDPipe.required "10" JD.string
        |> JDPipe.required "11" JD.string
        |> JDPipe.required "12" JD.string


genericWeekdayNamesDecoder : Decoder WeekdayNames
genericWeekdayNamesDecoder =
    JD.succeed WeekdayNames
        |> JDPipe.required "sun" JD.string
        |> JDPipe.required "mon" JD.string
        |> JDPipe.required "tue" JD.string
        |> JDPipe.required "wed" JD.string
        |> JDPipe.required "thu" JD.string
        |> JDPipe.required "fri" JD.string
        |> JDPipe.required "sat" JD.string


genericEraNamesDecoder : Decoder EraNames
genericEraNamesDecoder =
    JD.succeed EraNames
        |> JDPipe.required "0" JD.string
        |> JDPipe.required "1" JD.string


snakeIdentifier : LanguageInfo -> String
snakeIdentifier { language, script, territory, variant } =
    String.join "_"
        (List.filterMap identity
            [ Just language
            , script
            , territory
            , variant
            ]
        )
        |> fixReservedKeywords


reservedWords : Set String
reservedWords =
    Set.fromList
        [ "as" ]


fixReservedKeywords : String -> String
fixReservedKeywords word =
    if Set.member word reservedWords then
        "locale_" ++ word

    else
        word
