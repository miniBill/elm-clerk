module ToString exposing (..)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import IntTypes exposing (Value(..))
import Parser exposing (DeadEnd)
import Value


deadEndsToStrings : List DeadEnd -> List String
deadEndsToStrings deadEnds =
    deadEnds |> List.map deadEndToString


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
    "At row "
        ++ String.fromInt deadEnd.row
        ++ ", column "
        ++ String.fromInt deadEnd.col
        ++ ", problem : "
        ++ (case deadEnd.problem of
                Parser.Expecting string ->
                    "Expecting " ++ string

                Parser.ExpectingInt ->
                    "Expecting Int"

                Parser.ExpectingHex ->
                    "Expecting hex"

                Parser.ExpectingOctal ->
                    "Expecting Octal"

                Parser.ExpectingBinary ->
                    "Expecting Binary"

                Parser.ExpectingFloat ->
                    "Expecting Float"

                Parser.ExpectingNumber ->
                    "Expecting Number"

                Parser.ExpectingVariable ->
                    "Expecting Variable"

                Parser.ExpectingSymbol string ->
                    "Expecting symbol " ++ string

                Parser.ExpectingKeyword string ->
                    "Expecting keyword " ++ string

                Parser.ExpectingEnd ->
                    "Expecting end"

                Parser.UnexpectedChar ->
                    "Unexpected char"

                Parser.Problem string ->
                    "Problem: " ++ string

                Parser.BadRepeat ->
                    "BadRepeat"
           )


patternToString pattern =
    case pattern of
        AllPattern ->
            "AllPattern"

        UnitPattern ->
            "UnitPattern"

        CharPattern _ ->
            "Charpattern"

        StringPattern _ ->
            "Stringpattern"

        IntPattern _ ->
            "Intpattern"

        HexPattern _ ->
            "hexpattern"

        FloatPattern _ ->
            "floatpattern"

        TuplePattern _ ->
            "tuplepattern"

        RecordPattern _ ->
            "recordpattern"

        UnConsPattern _ _ ->
            "unconspattern"

        ListPattern _ ->
            "listpattern"

        VarPattern name ->
            "VarPattern: " ++ name

        NamedPattern _ _ ->
            "namedpattern"

        AsPattern _ _ ->
            "aspattern"

        ParenthesizedPattern _ ->
            "parenpattern"


expressionToString expression =
    case expression of
        UnitExpr ->
            "Unit"

        Application expressions ->
            "Application ["
                ++ (expressions
                        |> List.map Node.value
                        |> List.map expressionToString
                        |> String.join ", "
                   )
                ++ "]"

        OperatorApplication _ _ _ _ ->
            "OperatorApplication"

        FunctionOrValue moduleName name ->
            String.join "." (moduleName ++ [ name ])

        IfBlock _ _ _ ->
            "If"

        PrefixOperator _ ->
            "Prefix"

        Operator _ ->
            "Operator"

        Integer _ ->
            "Integer"

        Hex _ ->
            "Hex"

        Floatable _ ->
            "Floatable"

        Negation _ ->
            "Negation"

        Literal _ ->
            "Literal"

        CharLiteral _ ->
            "Charliteral"

        TupledExpression _ ->
            "Tupled"

        ParenthesizedExpression _ ->
            "ParenExpr"

        LetExpression _ ->
            "Let"

        CaseExpression _ ->
            "Case"

        LambdaExpression _ ->
            "Lambda"

        RecordExpr _ ->
            "RecordExpr"

        ListExpr _ ->
            "ListExpr"

        RecordAccess _ _ ->
            "RecordAccess"

        RecordAccessFunction _ ->
            "RecordAccess"

        RecordUpdateExpression _ _ ->
            "RecordUpdate"

        GLSLExpression _ ->
            "GLSL"


functionDeclarationToString : Value -> String
functionDeclarationToString value =
    case value of
        PartiallyApplied env values patterns maybeName (Node _ expression) ->
            [ "  Already applied:" ]
                ++ (values
                        |> List.map Value.toString
                   )
                ++ [ "  Pattern:" ]
                ++ (patterns
                        |> List.map Node.value
                        |> List.map patternToString
                   )
                ++ [ "  Name:" ]
                ++ (case maybeName of
                        Just name ->
                            name.moduleName
                                ++ [ name.name ]
                                |> String.join "."
                                |> List.singleton

                        Nothing ->
                            [ "" ]
                   )
                ++ [ " Expression:" ]
                ++ [ expressionToString expression ]
                |> String.join "\n"

        _ ->
            ""


evalErrorKindToString : IntTypes.EvalErrorKind -> String
evalErrorKindToString errorKind =
    case errorKind of
        IntTypes.TypeError string ->
            "TypeError: " ++ string

        IntTypes.Unsupported string ->
            "Unsupported: " ++ string

        IntTypes.NameError string ->
            "NameError: " ++ string

        IntTypes.Todo string ->
            "Todo: " ++ string
