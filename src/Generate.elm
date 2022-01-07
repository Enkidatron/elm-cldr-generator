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


localeFileDir : String
localeFileDir =
    "./elm-cldr/src/Cldr/Locale/"


mainGeneratedFileName : String
mainGeneratedFileName =
    "./elm-cldr/src/Internal/Generated.elm"


program : Process -> IO ()
program process =
    File.readDir baseDir
        |> IO.exitOnError identity
        |> IO.andThen (List.map checkIsDir >> IO.combine)
        |> reportAndFilterErrors identity
        |> andThenHelp loadGregorianJson
        |> reportAndFilterErrors identity
        |> mapHelp2 decodeFile
        |> reportAndFilterErrors JD.errorToString
        |> IO.map gatherByLanguage
        |> andThenHelp2 writeLocaleFile
        |> IO.map (List.concatMap Tuple.second)
        |> IO.andThen writeMainGeneratedFile


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


loadGregorianJson : String -> IO (Result String String)
loadGregorianJson dir =
    File.contentsOf (String.join "/" [ baseDir, dir, "ca-gregorian.json" ])


decodeFile : String -> String -> Result JD.Error LanguageInfo
decodeFile dirName contents =
    JD.decodeString (LanguageInfo.decoder dirName) contents


gatherByLanguage : List ( String, LanguageInfo ) -> List ( String, List LanguageInfo )
gatherByLanguage =
    List.map Tuple.second
        >> List.Extra.gatherEqualsBy .language
        >> List.map (\( first, rest ) -> ( String.Extra.toTitleCase first.language, first :: rest ))


writeLocaleFile : String -> List LanguageInfo -> IO (List LanguageInfo)
writeLocaleFile langFileName infos =
    File.writeContentsTo
        (localeFileDir ++ langFileName ++ ".elm")
        (CodeGen.localeFile langFileName infos)
        |> IO.map (always infos)


writeMainGeneratedFile : List LanguageInfo -> IO ()
writeMainGeneratedFile infos =
    File.writeContentsTo mainGeneratedFileName
        (CodeGen.mainGeneratedFile infos)


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
