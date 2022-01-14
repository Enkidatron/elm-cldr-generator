module Generate exposing (program)

import CodeGen
import Json.Decode as JD
import LanguageInfo exposing (LanguageInfo)
import List.Extra
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import String.Extra


baseDir : String
baseDir =
    "./cldr-json/cldr-json/cldr-dates-modern/main"


mainLocaleFileName : String
mainLocaleFileName =
    "./elm-cldr/src/Cldr/Locale.elm"


program : Process -> IO ()
program process =
    File.readDir baseDir
        |> IO.exitOnError identity
        |> IO.andThen (List.map checkIsDir >> IO.combine)
        |> reportAndFilterErrors identity
        |> andThenHelp loadGregorianJson
        |> reportAndFilterErrors identity
        |> log jsonLoadSuccessMessage
        |> mapHelp2 decodeFile
        |> reportAndFilterErrors JD.errorToString
        |> log jsonDecodeSuccessMessage
        |> IO.map gatherByLanguage
        |> IO.andThen writeMainLocaleFile
        |> IO.map (always ())
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


loadGregorianJson : String -> IO (Result String String)
loadGregorianJson dir =
    File.contentsOf (String.join "/" [ baseDir, dir, "ca-gregorian.json" ])


jsonLoadSuccessMessage : List a -> String
jsonLoadSuccessMessage jsonContents =
    "Successfully read " ++ String.fromInt (List.length jsonContents) ++ " ca-gregorian JSON files."


decodeFile : String -> String -> Result JD.Error LanguageInfo
decodeFile dirName contents =
    JD.decodeString (LanguageInfo.decoder dirName) contents


jsonDecodeSuccessMessage : List a -> String
jsonDecodeSuccessMessage infos =
    "Successfully decoded " ++ String.fromInt (List.length infos) ++ " locales."


gatherByLanguage : List ( String, LanguageInfo ) -> List ( String, List LanguageInfo )
gatherByLanguage =
    List.map Tuple.second
        >> List.Extra.gatherEqualsBy .language
        >> List.map (\( first, rest ) -> ( String.Extra.toTitleCase first.language, first :: rest ))


writeMainLocaleFile : List ( String, List LanguageInfo ) -> IO (List ( String, List LanguageInfo ))
writeMainLocaleFile infos =
    File.writeContentsTo mainLocaleFileName
        (CodeGen.mainLocaleFile infos)
        |> IO.map (always infos)


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
