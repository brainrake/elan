module LambdaEnv exposing (..)

import Lambda exposing (Exp(..))
import Env exposing (..)
import Html exposing (..)
import Lambda exposing (parse)


type Val
    = Closure (Env Val) String Exp


eval : Env Val -> Exp -> Val
eval env exp =
    case exp of
        Lam name body ->
            Closure env name body

        App fun arg ->
            case ( eval env fun, eval env arg ) of
                ( Closure funEnv funName funBody, Closure argEnv argName argBody ) ->
                    if funName == "" then
                        Closure [] "" (App funBody argBody)

                    else
                        eval (( funName, eval env arg ) :: funEnv) funBody

        Var name ->
            lookup name env |> Maybe.withDefault (Closure [] "" (Var name))


prelude : Env Val
prelude =
    [ ( "not", eval [] <| Lam "arg" (Lam "left" (Lam "right" (App (App (Var "arg") (Var "right")) (Var "left")))) )
    , ( "true", eval [] <| Lam "left" (Lam "right" (Var "right")) )
    , ( "false", eval [] <| Lam "left" (Lam "right" (Var "left")) )
    , ( "and", eval [] <| Lam "arg1" (Lam "arg2" (Lam "left" (Lam "right" (App (App (Var "arg2") (Var "left")) (App (App (Var "arg1") (Var "left")) (Var "right")))))) )
    ]


eval_ : Exp -> Val
eval_ =
    eval prelude


tests : List Bool
tests =
    [ eval_ (App (App (App (Var "not") (Var "false")) (Var "F")) (Var "T")) == eval_ (App (App (Var "true") (Var "F")) (Var "T"))
    , eval_ (App (App (App (App (Var "and") (Var "false")) (Var "false")) (Var "F")) (Var "T")) == eval_ (App (App (Var "false") (Var "F")) (Var "T"))
    , eval_ (App (App (App (App (Var "and") (Var "false")) (Var "true")) (Var "F")) (Var "T")) == eval_ (App (App (Var "false") (Var "F")) (Var "T"))
    , eval_ (App (App (App (App (Var "and") (Var "true")) (Var "false")) (Var "F")) (Var "T")) == eval_ (App (App (Var "false") (Var "F")) (Var "T"))
    , eval_ (App (App (App (App (Var "and") (Var "true")) (Var "true")) (Var "F")) (Var "T")) == eval_ (App (App (Var "true") (Var "F")) (Var "T"))
    ]


test_src : String
test_src =
    """
t1 = not false;
t2 = not (and false false);
t3 = not (and true false);
t4 = not (and false true);
t5 = and true true;
and t1 (and t2 (and t3 (and t4 (and t5 true)))) FALSE TRUE
"""


nat_src : String
nat_src =
    """
Y = f: (x: f (x x)) (x: f (x x));

## Natural Numbers
# Zero
zero = f: x: x;
# Successor
succ = n: f: x: f (n f x);

### Operations
pred = n: f: x: n (g: h: h (g f)) (u: x) (u: u);
plus = n: m: f: x: n f (m f x);
times = n: m: f: x: n (y: m f y) x;

-- eq_nat_ = eq_nat: m: n: m (u: n (v: eq_nat eq_nat (pred m) (pred n)) FALSE) (n (u: m (v: eq_nat eq_nat (pred m) (pred n)) FALSE) TRUE);
-- eq_nat = eq_nat_ eq_nat_;

one = succ zero;
two = succ one;
three = succ two;

test_six = times two three;
-- test_six = plus three three;

## Test
test_six S Z
-- eq_nat test_six (plus two (times two two))
-- a
-- one S Z
"""


example : Exp
example =
    App (Var "not") (Var "false")


main : Html.Html msg
main =
    -- [ Lambda.bool_src, Lambda.pair_src, Lambda.list_src, nat_src, test_src ]
    [ nat_src ]
        |> List.map parse
        |> List.map (eval prelude)
        |> Debug.toString
        |> Html.text
