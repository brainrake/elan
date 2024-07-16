module Lisp exposing (..)

import Env exposing (..)
import Html


type Exp
    = Lis (List Exp)
    | Atom String

apply : Env Exp -> Exp -> List Exp -> Exp
apply env fun args =
    Atom "TODO"



eval : Env Exp -> Exp -> Exp
eval env exp =
    case exp of
        Lis (x :: xs) ->
            apply env (eval env x) (List.map (eval env) xs)

        Lis [] ->
            Lis []

        Atom name ->
            lookup name env |> Maybe.withDefault exp


-- prelude : Env Exp
-- prelude =
--     [ "f"
--     ]


main : Html.Html msg
main =
    eval [] (Atom "a")
        |> Debug.toString
        |> Html.text
