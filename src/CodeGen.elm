module CodeGen exposing (localeFile, mainGeneratedFile)

import Elm.CodeGen as Gen
import Elm.Pretty
import Internal.Structures exposing (MonthNames, Patterns, WeekdayNames, mapPattern)
import LanguageInfo exposing (LanguageInfo, snakeIdentifier)


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
          , Gen.apply
                [ Gen.fun "Lang"
                , taggedStringExpr info.language
                , maybeTaggedStringExpr info.script
                , maybeTaggedStringExpr info.territory
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
        , ( "dateTokens"
          , weekdayNamesExpr dateFormatTokenListExpr
                (mapPattern parseDateFormatTokenBestEffort info.dateFormats)
          )
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
