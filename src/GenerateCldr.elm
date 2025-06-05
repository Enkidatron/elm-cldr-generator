module GenerateCldr exposing (program)

import CodeGen
import CurrencyDataInfo exposing (CurrencyDataInfo)
import DayPeriodsInfo exposing (DayPeriodsInfo)
import Internal.LanguageInfo exposing (LanguageInfo)
import Json.Decode as JD
import LanguageInfo.Extra
import List.Extra
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import StateMachine exposing (Looper(..))
import String.Extra


datesBaseDir : String
datesBaseDir =
    "./cldr-json/cldr-json/cldr-dates-modern/main"


numbersBaseDir : String
numbersBaseDir =
    "./cldr-json/cldr-json/cldr-numbers-modern/main"


dayPeriodsFileName : String
dayPeriodsFileName =
    "./cldr-json/cldr-json/cldr-core/supplemental/dayPeriods.json"


mainLocaleFileName : String
mainLocaleFileName =
    "./elm-cldr/src/Cldr/Locale.elm"


currencyDataFileName : String
currencyDataFileName =
    "./cldr-json/cldr-json/cldr-core/supplemental/currencyData.json"


currencyFileName : String
currencyFileName =
    "./elm-cldr/src/Generated/Currency.elm"


generatedDirName : String
generatedDirName =
    "./elm-cldr/src/Generated"


languageFileName : String -> String
languageFileName language =
    "./elm-cldr/src/Generated/" ++ String.Extra.toTitleCase language ++ ".elm"


program : Process -> IO ()
program =
    StateMachine.run (always Init) step


type StateMachine
    = Init
    | HaveCurrencyData CurrencyDataInfo
    | CurrencyDataFileWritten
    | HaveDayPeriods DayPeriodsInfo
    | HaveDateDirNames DayPeriodsInfo (List ( String, String ))
    | HaveDirNames DayPeriodsInfo (List ( String, String ))
    | HaveLanguageBundles DayPeriodsInfo (List ( String, List LanguageInfo ))
    | HaveWrittenLanguageBundles (List ( String, List LanguageInfo ))
    | MainLocaleFileIsWritten


step : StateMachine -> IO (Looper StateMachine)
step machine =
    case machine of
        Init ->
            File.contentsOf currencyDataFileName
                |> IO.exitOnError identity
                |> IO.map (JD.decodeString CurrencyDataInfo.decoder)
                |> IO.exitOnError JD.errorToString
                |> IO.map (HaveCurrencyData >> Continue)

        HaveCurrencyData currencyDataInfo ->
            writeCurrencyFile currencyDataInfo
                |> print ("Successfully wrote " ++ currencyFileName)
                |> IO.map (\_ -> Continue CurrencyDataFileWritten)

        CurrencyDataFileWritten ->
            File.contentsOf dayPeriodsFileName
                |> IO.exitOnError identity
                |> IO.map (JD.decodeString DayPeriodsInfo.decoder)
                |> IO.exitOnError JD.errorToString
                |> IO.map (HaveDayPeriods >> Continue)

        HaveDayPeriods dayPeriods ->
            File.readDir datesBaseDir
                |> IO.exitOnError identity
                |> IO.andThen (List.map checkIsDir >> IO.combine)
                |> reportAndFilterErrors identity
                |> IO.map (HaveDateDirNames dayPeriods >> Continue)

        HaveDateDirNames dayPeriods datesDirList ->
            File.readDir numbersBaseDir
                |> IO.exitOnError identity
                |> IO.andThen (List.map checkIsDir >> IO.combine)
                |> reportAndFilterErrors identity
                |> IO.andThen
                    (\numbersDirList ->
                        if numbersDirList == datesDirList then
                            IO.return (Continue (HaveDirNames dayPeriods datesDirList))

                        else
                            StateMachine.halt
                                |> print "ERROR: Dates and Numbers directories do not match"
                    )

        HaveDirNames dayPeriods dirList ->
            IO.return dirList
                |> andThenHelp loadAllJson
                |> reportAndFilterErrors identity
                |> log jsonLoadSuccessMessage
                |> mapHelp2 decodeFile
                |> reportAndFilterErrors JD.errorToString
                |> log jsonDecodeSuccessMessage
                |> IO.map gatherByLanguage
                |> log languageGroupMessage
                |> IO.map (HaveLanguageBundles dayPeriods >> Continue)

        HaveLanguageBundles dayPeriods languageBundles ->
            File.mkDir True generatedDirName
                |> IO.map (always languageBundles)
                |> andThenHelp2 (writeLanguageBundleFile dayPeriods)
                |> log languageFileWriteSuccessMessage
                |> IO.map
                    (always (HaveWrittenLanguageBundles languageBundles)
                        >> Continue
                    )

        HaveWrittenLanguageBundles languageBundles ->
            writeMainLocaleFile languageBundles
                |> IO.map (\_ -> Continue MainLocaleFileIsWritten)

        MainLocaleFileIsWritten ->
            StateMachine.halt
                |> print ("Successfully wrote " ++ mainLocaleFileName)
                |> print "*Reminder*"
                |> print "run these commands in `./elm-cldr` before committing to confirm everything worked properly:"
                |> print "elm-verify-examples"
                |> print "elm-test"
                |> print "elm-format ./"


andThenHelp : (a -> IO b) -> IO (List ( String, a )) -> IO (List ( String, b ))
andThenHelp nextIO =
    IO.andThen
        (List.map
            (\( dir, a ) ->
                nextIO a
                    |> IO.map (Tuple.pair dir)
            )
            >> IO.combine
        )


andThenHelp2 : (String -> a -> IO b) -> IO (List ( String, a )) -> IO (List ( String, b ))
andThenHelp2 nextIO =
    IO.andThen
        (List.map
            (\( dir, a ) ->
                nextIO dir a
                    |> IO.map (Tuple.pair dir)
            )
            >> IO.combine
        )


mapHelp2 : (String -> a -> b) -> IO (List ( String, a )) -> IO (List ( String, b ))
mapHelp2 fun =
    IO.map
        (List.map
            (\( dir, a ) ->
                ( dir, fun dir a )
            )
        )


checkIsDir : File.Entry -> IO ( String, Result String String )
checkIsDir entry =
    case entry of
        File.Directory dir ->
            IO.return ( dir, Ok dir )

        File.File name ->
            IO.return ( name, Err ("Unexpected file: " ++ name) )

        File.Other name ->
            IO.return ( name, Err ("Unexpected other: " ++ name) )


log : (a -> String) -> IO a -> IO a
log toLogMessage =
    IO.andThen (\a -> toLogMessage a |> Proc.print |> IO.map (always a))


print : String -> IO a -> IO a
print message =
    IO.andThen (\a -> Proc.print message |> IO.map (always a))


type alias AllJson =
    { gregorian : String
    , numbers : String
    , currencies : String
    }


loadAllJson : String -> IO (Result String AllJson)
loadAllJson dir =
    IO.do (loadGregorianJson dir) <|
        \gregorianResult ->
            IO.do (loadNumbersJson dir) <|
                \numbersResult ->
                    IO.do (loadCurrencyJson dir) <|
                        \currencyResult ->
                            IO.return (Result.map3 AllJson gregorianResult numbersResult currencyResult)


loadGregorianJson : String -> IO (Result String String)
loadGregorianJson dir =
    File.contentsOf (String.join "/" [ datesBaseDir, dir, "ca-gregorian.json" ])


loadNumbersJson : String -> IO (Result String String)
loadNumbersJson dir =
    File.contentsOf (String.join "/" [ numbersBaseDir, dir, "numbers.json" ])


loadCurrencyJson : String -> IO (Result String String)
loadCurrencyJson dir =
    File.contentsOf (String.join "/" [ numbersBaseDir, dir, "currencies.json" ])


jsonLoadSuccessMessage : List a -> String
jsonLoadSuccessMessage jsonContents =
    "Successfully read " ++ String.fromInt (List.length jsonContents) ++ " ca-gregorian, numbers, and currencies JSON files."


decodeFile : String -> AllJson -> Result JD.Error LanguageInfo
decodeFile dirName contents =
    JD.decodeString (LanguageInfo.Extra.decoder dirName) <|
        String.concat
            [ "{\"gregorian\": "
            , contents.gregorian
            , ", \"numbers\": "
            , contents.numbers
            , ", \"currencies\": "
            , contents.currencies
            , "}"
            ]


jsonDecodeSuccessMessage : List a -> String
jsonDecodeSuccessMessage infos =
    "Successfully decoded " ++ String.fromInt (List.length infos) ++ " locales."


gatherByLanguage : List ( String, LanguageInfo ) -> List ( String, List LanguageInfo )
gatherByLanguage =
    List.map Tuple.second
        >> List.Extra.gatherEqualsBy .language
        >> List.map (\( first, rest ) -> ( String.Extra.toTitleCase first.language, first :: rest ))


languageGroupMessage : List a -> String
languageGroupMessage grouped =
    "Grouped locales into " ++ String.fromInt (List.length grouped) ++ " languages."


writeLanguageBundleFile : DayPeriodsInfo -> String -> List LanguageInfo -> IO ()
writeLanguageBundleFile dayPeriods languageTag infos =
    File.writeContentsTo (languageFileName languageTag)
        (CodeGen.languageFile languageTag dayPeriods infos)


languageFileWriteSuccessMessage : List a -> String
languageFileWriteSuccessMessage grouped =
    "Successfully wrote " ++ String.fromInt (List.length grouped) ++ " language files."


writeMainLocaleFile : List ( String, List LanguageInfo ) -> IO (List ( String, List LanguageInfo ))
writeMainLocaleFile infos =
    File.writeContentsTo mainLocaleFileName
        (CodeGen.mainLocaleFile infos)
        |> IO.map (always infos)


writeCurrencyFile : CurrencyDataInfo -> IO ()
writeCurrencyFile info =
    File.writeContentsTo currencyFileName
        (CodeGen.currencyFile info)


reportAndFilterErrors : (e -> String) -> IO (List ( String, Result e a )) -> IO (List ( String, a ))
reportAndFilterErrors errToString =
    IO.andThen
        (List.map
            (\( dirName, result ) ->
                case result of
                    Ok ok ->
                        IO.return (Just ( dirName, ok ))

                    Err err ->
                        Proc.print ("Error in " ++ dirName ++ "\n" ++ errToString err)
                            |> IO.map (always Nothing)
            )
            >> IO.combine
            >> IO.map (List.filterMap identity)
        )
