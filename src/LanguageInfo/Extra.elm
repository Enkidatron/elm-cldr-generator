module LanguageInfo.Extra exposing (decoder, skewerCase, snakeIdentifier)

import Internal.LanguageInfo exposing (LanguageInfo)
import Internal.Structures exposing (EraNames, MonthNames, Pattern3, Patterns, PeriodNames, WeekdayNames)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDPipe
import Set exposing (Set)


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
        |> JDPipe.custom periodNamesDecoder
        |> JDPipe.custom datePatternsDecoder
        |> JDPipe.custom monthFormatNamesDecoder
        |> JDPipe.custom monthStandaloneNamesDecoder
        |> JDPipe.custom weekdayFormatNamesDecoder
        |> JDPipe.custom weekdayStandaloneNamesDecoder
        |> JDPipe.custom eraNamesDecoder
        |> JDPipe.custom timePatternsDecoder
        |> JDPipe.custom dateTimePatternsDecoder
        |> JDPipe.custom availableFormatsDecoder
        |> JDPipe.custom timeSkeletonsDecoder


periodNamesDecoder : Decoder (Pattern3 PeriodNames)
periodNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "dayPeriods", "format" ]
        (pattern3Decoder genericPeriodNamesDecoder)


genericPeriodNamesDecoder : Decoder PeriodNames
genericPeriodNamesDecoder =
    JD.succeed PeriodNames
        |> JDPipe.required "am" JD.string
        |> JDPipe.required "pm" JD.string
        |> JDPipe.custom (JD.dict JD.string)


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


availableFormatsDecoder : Decoder (List ( String, String ))
availableFormatsDecoder =
    JD.at [ "dates", "calendars", "gregorian", "dateTimeFormats", "availableFormats" ]
        (JD.keyValuePairs JD.string)


timeSkeletonsDecoder : Decoder (Patterns String)
timeSkeletonsDecoder =
    JD.at [ "dates", "calendars", "gregorian", "timeSkeletons" ]
        (patternDecoder JD.string)


monthFormatNamesDecoder : Decoder (Pattern3 MonthNames)
monthFormatNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "months", "format" ]
        (pattern3Decoder genericMonthNamesDecoder)


monthStandaloneNamesDecoder : Decoder (Pattern3 MonthNames)
monthStandaloneNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "months", "stand-alone" ]
        (pattern3Decoder genericMonthNamesDecoder)


weekdayFormatNamesDecoder : Decoder (Pattern3 WeekdayNames)
weekdayFormatNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "days", "format" ]
        (pattern3Decoder genericWeekdayNamesDecoder)


weekdayStandaloneNamesDecoder : Decoder (Pattern3 WeekdayNames)
weekdayStandaloneNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "days", "stand-alone" ]
        (pattern3Decoder genericWeekdayNamesDecoder)


eraNamesDecoder : Decoder (Pattern3 EraNames)
eraNamesDecoder =
    JD.at [ "dates", "calendars", "gregorian", "eras" ] <|
        JD.map3 Pattern3
            (JD.field "eraAbbr" genericEraNamesDecoder)
            (JD.field "eraNames" genericEraNamesDecoder)
            (JD.field "eraNarrow" genericEraNamesDecoder)


patternDecoder : Decoder a -> Decoder (Patterns a)
patternDecoder innerDecoder =
    JD.succeed Patterns
        |> JDPipe.required "short" innerDecoder
        |> JDPipe.required "medium" innerDecoder
        |> JDPipe.required "long" innerDecoder
        |> JDPipe.required "full" innerDecoder


pattern3Decoder : Decoder a -> Decoder (Pattern3 a)
pattern3Decoder innerDecoder =
    JD.succeed Pattern3
        |> JDPipe.required "abbreviated" innerDecoder
        |> JDPipe.required "wide" innerDecoder
        |> JDPipe.required "narrow" innerDecoder


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


skewerCase : LanguageInfo -> String
skewerCase { language, script, territory, variant } =
    String.join "-"
        (List.filterMap identity
            [ Just language
            , script
            , territory
            , variant
            ]
        )


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
