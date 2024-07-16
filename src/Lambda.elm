module Lambda exposing (..)

import Env exposing (..)
import Html
import Html.Attributes exposing (value)
import Parser exposing ((|.), (|=), Trailing(..), andThen, backtrackable, end, lazy, lineComment, oneOf, spaces, succeed, symbol, token, variable)
import Set
import Util exposing (..)


type Exp
    = Lam String Exp
    | App Exp Exp
    | Var String


rename : String -> String -> Exp -> Exp
rename from to exp =
    case exp of
        Lam name body ->
            if name == from then
                exp

            else
                Lam name (rename from to body)

        App fun arg ->
            App (rename from to fun) (rename from to arg)

        Var name ->
            if name == from then
                Var to

            else
                exp


fresh : String -> Exp -> String
fresh name exp =
    case exp of
        Lam n body ->
            if n == name then
                name

            else
                fresh name body

        App fun arg ->
            case ( fresh name fun, fresh name arg ) of
                ( x, y ) ->
                    if String.length x >= String.length y then
                        x

                    else
                        y

        Var n ->
            if n == name then
                name ++ "_"

            else
                name


substitute : String -> Exp -> Exp -> Exp
substitute name value exp =
    case exp of
        Lam n body ->
            if n == name then
                exp

            else
                let
                    newName =
                        fresh n value

                    renamedBody =
                        rename n newName body

                    substitutedbody =
                        substitute name value renamedBody
                in
                Lam newName substitutedbody

        App fun arg ->
            App (substitute name value fun) (substitute name value arg)

        Var n ->
            if n == name then
                value

            else
                exp


eval : Exp -> Exp
eval exp =
    case exp of
        Lam name body ->
            Lam name (eval body)

        App fun arg ->
            case fun of
                Lam name body ->
                    substitute name arg body
                _ ->
                    App (eval fun) (eval arg)

        Var _ ->
            exp


eval_ : Exp -> Exp
eval_ =
    fix eval


prelude : Env Exp
prelude =
    [ ( "not", Lam "arg" (Lam "left" (Lam "right" (App (App (Var "arg") (Var "right")) (Var "left")))) )
    , ( "true", Lam "left" (Lam "right" (Var "right")) )
    , ( "false", Lam "left" (Lam "right" (Var "left")) )
    , ( "and", Lam "arg1" (Lam "arg2" (Lam "left" (Lam "right" (App (App (Var "arg1") (Var "left")) (App (App (Var "arg2") (Var "left")) (Var "right")))))) )
    , ( "or", Lam "arg1" (Lam "arg2" (Lam "left" (Lam "right" (App (App (Var "arg1") (App (App (Var "arg2") (Var "left")) (Var "right"))) (Var "right"))))) )
    , ( "if", Lam "condition" (Lam "then-branch" (Lam "else-branch" (App (App (Var "condition") (Var "else-branch")) (Var "then-branch")))) )
    ]


p : String -> Exp
p name =
    lookup name prelude |> Maybe.withDefault (Var "name-error")


withPrelude : Exp -> Exp
withPrelude exp =
    List.foldr (\( n, v ) e -> App (Lam n e) v) exp prelude


example : Exp
example =
    App (p "not") (p "false")


tests : List Bool
tests =
    [ eval_ (App (p "not") (p "false")) == eval_ (p "true")
    , eval_ (App (App (p "and") (p "false")) (p "false")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "false")) (p "true")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "true")) (p "false")) == eval_ (p "false")
    , eval_ (App (App (p "and") (p "true")) (p "true")) == eval_ (p "true")
    ]


spc : Parser.Parser ()
spc =
    succeed ()
        |. spaces
        |. oneOf
            [ succeed ()
                |. oneOf [ lineComment "#", lineComment "--" ]
                |. lazy (\_ -> spc)
            , succeed ()
            ]


parser : Parser.Parser Exp
parser =
    let
        name =
            variable
                { start = Char.isAlpha
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }

        -- app_rest : () -> Parser.Parser (List Exp)
        app_rest _ =
            succeed (::)
                |. token " "
                |= nonApp
                |= lazy (\_ -> oneOf [ app_rest (), succeed [] ])

        app =
            succeed Tuple.pair
                |= nonApp
                |= lazy app_rest
                |> andThen (\( x, xs ) -> succeed (List.foldl (\z a -> App a z) x xs))

        nonApp =
            oneOf
                [ succeed identity
                    |= name
                    |> andThen
                        (\n ->
                            oneOf
                                [ succeed (\e -> Lam n e)
                                    |. backtrackable spc
                                    |. symbol ":"
                                    |. spc
                                    |= lazy (\_ -> parser)
                                , succeed (\e1 e2 -> App (Lam n e2) e1)
                                    |. backtrackable spc
                                    |. symbol "="
                                    |. spc
                                    |= lazy (\_ -> parser)
                                    |. symbol ";"
                                    |. spc
                                    |= lazy (\_ -> parser)
                                , succeed (Var n)
                                ]
                        )
                , succeed identity
                    |. symbol "("
                    |= lazy (\_ -> parser)
                    |. symbol ")"
                ]
    in
    oneOf
        [ backtrackable app
        , nonApp
        ]


parse : String -> Exp
parse str =
    let
        result =
            Parser.run
                (succeed identity
                    |. spc
                    |= parser
                    |. spc
                    |. end
                )
                str
    in
    case result of
        Ok ok ->
            ok

        Err err ->
            Var ("Error: " ++ Debug.toString err)


bool_src : String
bool_src =
    """



-- true =  \\l . \\r . r



true= l: r: r;
false= l: r: l;
not= x: l: r: x r l;
and= x: y: l: r: x l (y l r);
or= x: y: l: r: x (y l r) r;
eq= x: y: l: r: x (y r l) (y l r);

test1= not false;
test2= not (not (not false));
test3= not (not true);
test4= and true true;
tests= and (and (and test1 test2) test3) test4;
tests FALSE TRUE
"""


pair_src : String
pair_src =
    """
pair = x: y: f: f x y;
fst = p: p (x: y: x);
snd = p: p (x: y: y);

curry = f: x: y: f (pair x y);
uncurry = f: p: f (fst p) (snd p);

test1 = fst (snd (pair FALSE (pair TRUE FALSE)));

(curry (uncurry (x: y: x))) TRUE FALSE
"""

type alias Nat a = (a -> a) -> a -> a


zero : Nat a
zero = \_ -> \x -> x

succ : Nat a ->Nat a
succ = \pred -> \f -> \x -> f (pred f x)


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
times = n: m: f: x: n (m f) x;

eq_nat_ = eq_nat: m: n: m (u: n (v: eq_nat (pred m) (pred n)) FALSE) (n (u: m (v: eq_nat (pred m) (pred n)) FALSE) TRUE);
eq_nat = Y eq_nat_;

one = succ zero;
two = succ one;
three = succ two;

test_six = times two three;

## Test
eq_nat test_six (plus two (times two two))
"""


list_src : String
list_src =
    """
cons = x: xs: cons: nil: cons x xs;
nil = cons: nil: nil;

head = l: l (x: xs: x) nil;
tail = l: l (x: xs: xs) nil;

map = f: l: l (x: xs: cons (f x) (map f xs)) nil;

test1 = head (tail (tail (cons FALSE (cons FALSE (cons TRUE nil)))));

test2 = map succ (cons zero (cons one nil));

(map (x: x S Z) test2) C N
"""

lam_src : String
lam_src =
  """
lam = arg: exp: l: a: v: l arg exp
app = fun: arg: l: a: v: a fun arg
var = name: l: a: v: v name

rename = z;
substitute = z;
fresh = z;
eval = z;

z
"""


main : Html.Html msg
main =
    [ bool_src, pair_src, nat_src, list_src ]
        |> List.map parse
        |> List.map eval_
        |> Debug.toString
        |> Html.text
