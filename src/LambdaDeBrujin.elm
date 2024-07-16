module LambdaDeBrujin exposing (..)

import Env exposing (..)
import Html
import Html.Attributes exposing (value)
import Util exposing (..)


type Exp
    = Lam Exp
    | App Exp Exp
    | Var Int


toString : Exp -> String
toString exp =
    case exp of
        Lam e ->
            "\\{" ++ toString e ++ "}"

        App e1 e2 ->
            "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"

        Var i ->
            String.fromInt i


updateIndices : Int -> Int -> Exp -> Exp
updateIndices index depth exp =
    case exp of
        Lam body ->
            Lam (updateIndices index (depth + 1) body)

        App fun arg ->
            App (updateIndices index depth fun) (updateIndices index depth arg)

        Var i ->
            if i >= depth then
                Var (i + index)

            else
                exp


substitute : Int -> Exp -> Exp -> Exp
substitute index value exp =
    case exp of
        Lam body ->
            Lam (substitute (index + 1) value body)

        App fun arg ->
            App (substitute index value fun) (substitute index value arg)

        Var i ->
            if i == index then
                updateIndices index 0 value

            else if i > index then
                Var (i - 1)

            else
                Var i


eval : Exp -> Exp
eval exp =
    case exp of
        Lam body ->
            Lam (eval body)

        App fun arg ->
            case fun of
                Lam body ->
                    substitute 0 arg body

                _ ->
                    App (eval fun) arg

        Var _ ->
            exp


eval_ : Exp -> Exp
eval_ =
    fix eval


prelude : Env Exp
prelude =
    [ ( "true", Lam (Lam (Var 0)) )
    , ( "false", Lam (Lam (Var 1)) )
    , ( "not", Lam (Lam (Lam (App (App (Var 2) (Var 0)) (Var 1)))) )
    , ( "and", Lam (Lam (Lam (Lam (App (App (Var 2) (Var 1)) (App (App (Var 3) (Var 1)) (Var 0)))))) )
    ]


p : String -> Exp
p name =
    prelude
        |> lookup name
        |> Maybe.withDefault (Var -9999)


example : Exp
example =
    App (p "not") (App (p "not") (p "false"))



-- App (App (Var "and") (Var "true")) (Var "true")
-- Var "true"


tests : List Bool
tests =
    [ eval_ (App (p "not") (p "false")) == eval_ (p "true")
    , eval_ (App (App (p "and") (p "false")) (p "false")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "false")) (p "true")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "true")) (p "false")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "true")) (p "true")) == eval_ (p "true")
    ]


main : Html.Html msg
main =
    tests
        |> Debug.toString
        |> Html.text
