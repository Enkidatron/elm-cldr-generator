module CodeGen.FormatSymbols exposing
    ( dateWithLiteral
    , formatWithLiteral
    , timeWithLiteral
    )

import Elm.CodeGen as Gen
import Internal.FormatSymbols exposing (..)


withLiteral : (a -> Gen.Expression) -> WithLiteral a -> Gen.Expression
withLiteral innerFun withLit =
    case mapLiteral innerFun withLit of
        Symbol innerExpr ->
            applySym "Symbol" (Gen.parens innerExpr)

        Literal words ->
            applySym "Literal" (Gen.string words)


applySym : String -> Gen.Expression -> Gen.Expression
applySym funName innerExpr =
    Gen.apply
        [ Gen.fqFun [ "Sym" ] funName
        , innerExpr
        ]


sym : String -> Gen.Expression
sym =
    Gen.fqVal [ "Sym" ]


formatWithLiteral : FormatWithLiteral -> Gen.Expression
formatWithLiteral =
    withLiteral format


format : FormatSymbol -> Gen.Expression
format symbol =
    case symbol of
        Time timeSymbol ->
            applySym "Time" (Gen.parens (time timeSymbol))

        Date dateSymbol ->
            applySym "Date" (Gen.parens (date dateSymbol))


timeWithLiteral : TimeWithLiteral -> Gen.Expression
timeWithLiteral =
    withLiteral time


time : TimeSymbol -> Gen.Expression
time symbol =
    case symbol of
        Period textWidth ->
            applySym "Period" (textWidthGen textWidth)

        FlexibleDayPeriod textWidth ->
            applySym "FlexibleDayPeriod" (textWidthGen textWidth)

        Hour12From1 numberWidth ->
            applySym "Hour12From1" (numberWidthGen numberWidth)

        Hour24From0 numberWidth ->
            applySym "Hour24From0" (numberWidthGen numberWidth)

        Hour12From0 numberWidth ->
            applySym "Hour12From0" (numberWidthGen numberWidth)

        Hour24From1 numberWidth ->
            applySym "Hour24From1" (numberWidthGen numberWidth)

        Minute numberWidth ->
            applySym "Minute" (numberWidthGen numberWidth)

        Second numberWidth ->
            applySym "Second" (numberWidthGen numberWidth)

        FractionalSeconds digits ->
            applySym "FractionalSeconds" (Gen.int digits)

        ZoneNonLocationFormat nameWidth ->
            applySym "ZoneNonLocationFormat" (nameWidthGen nameWidth)

        ZoneIso8601Basic ->
            sym "ZoneIso8601Basic"

        ZoneGmtFormat nameWidth ->
            applySym "ZoneGmtFormat" (nameWidthGen nameWidth)

        ZoneGenericNonLocationFormat nameWidth ->
            applySym "ZoneGenericNonLocationFormat" (nameWidthGen nameWidth)


dateWithLiteral : DateWithLiteral -> Gen.Expression
dateWithLiteral =
    withLiteral date


date : DateSymbol -> Gen.Expression
date symbol =
    case symbol of
        Era textWidth ->
            applySym "Era" (textWidthGen textWidth)

        Year numberWidth ->
            applySym "Year" (numberWidthGen numberWidth)

        Month width ->
            applySym "Month" (Gen.parens (widthGen width))

        MonthStandalone width ->
            applySym "MonthStandalone" (Gen.parens (widthGen width))

        Weekday textWidth ->
            applySym "Weekday" (textWidthGen textWidth)

        WeekdayStandalone textWidth ->
            applySym "WeekdayStandalone" (textWidthGen textWidth)

        Day numberWidth ->
            applySym "Day" (numberWidthGen numberWidth)


textWidthGen : TextWidth -> Gen.Expression
textWidthGen textWidth =
    case textWidth of
        Abbreviated ->
            sym "Abbreviated"

        Wide ->
            sym "Wide"

        Narrow ->
            sym "Narrow"


numberWidthGen : NumberWidth -> Gen.Expression
numberWidthGen numberWidth =
    case numberWidth of
        MinimumDigits ->
            sym "MinimumDigits"

        TwoDigit ->
            sym "TwoDigit"


nameWidthGen : NameWidth -> Gen.Expression
nameWidthGen nameWidth =
    case nameWidth of
        Short ->
            sym "Short"

        Long ->
            sym "Long"


widthGen : Width -> Gen.Expression
widthGen width =
    case width of
        Text textWidth ->
            applySym "Text" (textWidthGen textWidth)

        Number numberWidth ->
            applySym "Number" (numberWidthGen numberWidth)
