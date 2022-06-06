module Benchy exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Cldr.Format.Date
import Cldr.Format.Length
import Cldr.Format.Options
import Cldr.Locale
import Cldr.LocaleAlt
import Date exposing (Date)
import Time


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    Benchmark.describe ""
        [ lookupSuite
        , formatSuite
        ]


lookupSuite : Benchmark
lookupSuite =
    Benchmark.describe "Lookups"
        [ lookupEnUsFromSingle
        , lookupEnUsFromBigList
        , lookupNonsenseFromSingle
        ]


lookupEnUsFromSingle : Benchmark
lookupEnUsFromSingle =
    Benchmark.compare "en_US from single item list"
        "Current"
        (\_ -> Cldr.Locale.fromString [ Cldr.Locale.en ] "en_US")
        "Alternative"
        (\_ -> Cldr.Locale.fromString [ Cldr.LocaleAlt.en ] "en_US")


lookupEnUsFromBigList : Benchmark
lookupEnUsFromBigList =
    Benchmark.compare "en_US from allLocales"
        "Current"
        (\_ -> Cldr.Locale.fromString Cldr.Locale.allLocales "en_US")
        "Alternative"
        (\_ -> Cldr.Locale.fromString Cldr.LocaleAlt.allLocales "en_US")


lookupNonsenseFromSingle : Benchmark
lookupNonsenseFromSingle =
    Benchmark.compare "nonsense from single item list"
        "Current"
        (\_ -> Cldr.Locale.fromString [ Cldr.Locale.en ] "nonsense")
        "Alternative"
        (\_ -> Cldr.Locale.fromString [ Cldr.LocaleAlt.en ] "nonsense")


formatSuite : Benchmark
formatSuite =
    Benchmark.describe "Format Dates"
        [ formatEnUsShort
        , formatEnUsOptions
        ]


testDate : Date
testDate =
    Date.fromCalendarDate 2022 Time.Jun 1


formatEnUsShort : Benchmark
formatEnUsShort =
    Benchmark.compare "format en_US short"
        "Current"
        (\_ ->
            Cldr.Format.Date.format (Cldr.Format.Date.WithLength Cldr.Format.Length.Short)
                Cldr.Locale.en
                testDate
        )
        "Alternative"
        (\_ ->
            Cldr.Format.Date.format (Cldr.Format.Date.WithLength Cldr.Format.Length.Short)
                Cldr.LocaleAlt.en
                testDate
        )


formatEnUsOptions : Benchmark
formatEnUsOptions =
    Benchmark.compare "format en_US options"
        "Current"
        (\_ ->
            Cldr.Format.Date.format
                (Cldr.Format.Date.WithOptions
                    { era = Just Cldr.Format.Options.Short
                    , year = Just Cldr.Format.Options.Numeric
                    , month = Just (Cldr.Format.Options.Text Cldr.Format.Options.Short)
                    , day = Just Cldr.Format.Options.Numeric
                    , weekday = Just Cldr.Format.Options.Short
                    }
                )
                Cldr.Locale.en
                testDate
        )
        "Alternative"
        (\_ ->
            Cldr.Format.Date.format
                (Cldr.Format.Date.WithOptions
                    { era = Just Cldr.Format.Options.Short
                    , year = Just Cldr.Format.Options.Numeric
                    , month = Just (Cldr.Format.Options.Text Cldr.Format.Options.Short)
                    , day = Just Cldr.Format.Options.Numeric
                    , weekday = Just Cldr.Format.Options.Short
                    }
                )
                Cldr.LocaleAlt.en
                testDate
        )
