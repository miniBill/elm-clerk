module ToString exposing (..)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import InterpreterTypes exposing (Value(..))
import Parser exposing (DeadEnd)
import Value


listToString : String -> String -> List String -> String
listToString left right list =
    left ++ String.join ", " list ++ right


listToStringDebug : String -> List String -> String
listToStringDebug name list =
    name ++ " [" ++ String.join ", " list ++ "]"


listToStringParenDebug : String -> List String -> String
listToStringParenDebug name list =
    name ++ " (" ++ String.join ", " list ++ ")"


qualifiedNameRefToString : QualifiedNameRef -> String
qualifiedNameRefToString name =
    name.moduleName
        ++ [ name.name ]
        |> String.join "."


functionDeclarationToString : Value -> String
functionDeclarationToString value =
    case value of
        PartiallyApplied _ values patterns maybeName (Node _ expression) ->
            "  Already applied:"
                :: (values
                        |> List.map Value.toString
                   )
                ++ [ "  Pattern:" ]
                ++ (patterns
                        |> List.map Node.value
                        |> List.map patternToStringDebug
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


evalErrorKindToString : InterpreterTypes.EvalErrorKind -> String
evalErrorKindToString errorKind =
    case errorKind of
        InterpreterTypes.TypeError string ->
            "TypeError: " ++ string

        InterpreterTypes.Unsupported string ->
            "Unsupported: " ++ string

        InterpreterTypes.NameError string ->
            "NameError: " ++ string

        InterpreterTypes.Todo string ->
            "Todo: " ++ string


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


annotationToStringsDebug : TypeAnnotation -> List String
annotationToStringsDebug annotation =
    case annotation of
        TypeAnnotation.GenericType a ->
            [ "Generic " ++ a ]

        TypeAnnotation.Typed (Node _ ( moduleName, name )) children ->
            String.join "." (moduleName ++ [ name ])
                :: (children
                        |> List.map Node.value
                        |> List.concatMap annotationToStringsDebug
                   )

        TypeAnnotation.Unit ->
            [ "()" ]

        TypeAnnotation.Tupled list ->
            list
                |> List.map Node.value
                |> List.concatMap annotationToStringsDebug

        TypeAnnotation.Record _ ->
            [ "Record" ]

        TypeAnnotation.GenericRecord _ _ ->
            [ "GenericRecord" ]

        TypeAnnotation.FunctionTypeAnnotation first second ->
            (first |> Node.value |> annotationToStringsDebug)
                ++ (second |> Node.value |> annotationToStringsDebug)


annotationToString : TypeAnnotation -> String
annotationToString annotation =
    case annotation of
        TypeAnnotation.GenericType generic ->
            generic

        -- `Typed`: `Maybe (Int -> String)`
        --  | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
        TypeAnnotation.Typed (Node _ ( moduleName, name )) children ->
            String.join "." (moduleName ++ [ name ])
                :: (children
                        |> List.map Node.value
                        |> List.map annotationToString
                   )
                |> String.join " "

        TypeAnnotation.Unit ->
            "()"

        TypeAnnotation.Tupled list ->
            list
                |> List.map Node.value
                |> List.concatMap annotationToStringsDebug
                |> listToString "(" ")"

        TypeAnnotation.Record _ ->
            "{{Record}}"

        TypeAnnotation.GenericRecord _ _ ->
            "{{GenericRecord}}"

        TypeAnnotation.FunctionTypeAnnotation first second ->
            (first |> Node.value |> annotationToString)
                ++ " -> "
                ++ (second |> Node.value |> annotationToString)


patternToString pattern =
    case pattern of
        AllPattern ->
            "_"

        UnitPattern ->
            "()"

        CharPattern char ->
            "'" ++ String.fromChar char ++ "'"

        StringPattern string ->
            "\"" ++ string ++ "\""

        IntPattern int ->
            String.fromInt int

        HexPattern hex ->
            "hex-" ++ String.fromInt hex

        FloatPattern float ->
            String.fromFloat float

        TuplePattern patterns ->
            "("
                ++ (patterns
                        |> List.map Node.value
                        |> List.map patternToString
                        |> String.join ", "
                   )
                ++ ")"

        RecordPattern record ->
            record
                |> List.map Node.value
                |> listToString "{" "}"

        UnConsPattern first second ->
            (first
                |> Node.value
                |> patternToString
            )
                ++ " :: "
                ++ (second
                        |> Node.value
                        |> patternToString
                   )

        ListPattern list ->
            list
                |> List.map Node.value
                |> List.map patternToString
                |> listToString "[" "]"

        VarPattern name ->
            name

        NamedPattern name list ->
            qualifiedNameRefToString name
                ++ (list
                        |> List.map Node.value
                        |> List.map patternToString
                        |> String.join " "
                   )

        AsPattern first second ->
            (first
                |> Node.value
                |> patternToString
            )
                ++ " as "
                ++ (second |> Node.value)

        ParenthesizedPattern inner ->
            "(" ++ (inner |> Node.value |> patternToString) ++ ")"


patternToStringDebug pattern =
    case pattern of
        AllPattern ->
            "AllPattern"

        UnitPattern ->
            "UnitPattern"

        CharPattern char ->
            "CharPattern " ++ String.fromChar char

        StringPattern string ->
            "StringPattern " ++ string

        IntPattern int ->
            "IntPattern " ++ String.fromInt int

        HexPattern hex ->
            "HexPattern " ++ String.fromInt hex

        FloatPattern float ->
            "FloatPattern " ++ String.fromFloat float

        TuplePattern patterns ->
            patterns
                |> List.map Node.value
                |> List.map patternToStringDebug
                |> listToStringParenDebug "TuplePattern"

        RecordPattern record ->
            record
                |> List.map Node.value
                |> listToStringDebug "RecordPattern"

        UnConsPattern first second ->
            "UnConsPattern"
                ++ (first
                        |> Node.value
                        |> patternToStringDebug
                   )
                ++ " "
                ++ (second
                        |> Node.value
                        |> patternToStringDebug
                   )

        ListPattern list ->
            list
                |> List.map Node.value
                |> List.map patternToStringDebug
                |> listToStringParenDebug "ListPattern"

        VarPattern name ->
            "VarPattern: " ++ name

        NamedPattern name list ->
            "NamedPattern"
                ++ qualifiedNameRefToString name
                ++ (list
                        |> List.map Node.value
                        |> List.map patternToStringDebug
                        |> listToStringParenDebug ""
                   )

        AsPattern first second ->
            "AsPattern"
                ++ (first
                        |> Node.value
                        |> patternToStringDebug
                   )
                ++ " "
                ++ (second |> Node.value)

        ParenthesizedPattern inner ->
            "ParenPattern (" ++ (inner |> Node.value |> patternToStringDebug) ++ ")"


expressionToString expression =
    case expression of
        UnitExpr ->
            "Unit"

        Application expressions ->
            expressions
                |> List.map Node.value
                |> List.map expressionToString
                |> listToStringDebug "Application"

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
