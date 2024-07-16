module Hvm exposing (..)

import Html exposing (Html, text)
import Tuple exposing (first, second)


type alias Name =
    String


type Node
    = Era
    | Ref Name
    | Num Int
    | Con Tree Tree
    | Dup Tree Tree
    | Ope Tree Tree
    | Swi Tree Tree


type Tree
    = Var Name
    | Node Node


type alias Redex =
    ( Tree, Tree )


type alias Net =
    ( Tree, List Redex )


type alias Book =
    List ( Name, Net )


dup : Tree -> Tree -> Tree
dup x y =
    Node (Dup x y)


con : Tree -> Tree -> Tree
con x y =
    Node (Con x y)


ope : Tree -> Tree -> Tree
ope x y =
    Node (Ope x y)


swi : Tree -> Tree -> Tree
swi x y =
    Node (Swi x y)


example : Book
example =
    [ ( "succ", ( con (dup (con (Var "a") (Var "b")) (con (Var "b") (Var "R"))) (con (Var "a") (Var "R")), [] ) )
    ]


nullary : Tree -> Bool
nullary tree =
    case tree of
        Node Era ->
            True

        Node (Ref _) ->
            True

        Node (Num _) ->
            True

        _ ->
            False


binary : Tree -> Bool
binary =
    not << nullary


branches : Tree -> Maybe ( Tree, Tree )
branches t =
    case t of
        Node (Con t1 t2) ->
            Just ( t1, t2 )

        Node (Dup t1 t2) ->
            Just ( t1, t2 )

        Node (Ope t1 t2) ->
            Just ( t1, t2 )

        Node (Swi t1 t2) ->
            Just ( t1, t2 )

        _ ->
            Nothing


unbinary : Tree -> Maybe ( Tree -> Tree -> Tree, Tree, Tree )
unbinary t =
    case t of
        Node (Con t1 t2) ->
            Just ( con, t1, t2 )

        Node (Dup t1 t2) ->
            Just ( dup, t1, t2 )

        Node (Ope t1 t2) ->
            Just ( ope, t1, t2 )

        Node (Swi t1 t2) ->
            Just ( swi, t1, t2 )

        _ ->
            Nothing


same_binary : Redex -> Maybe ( ( Tree, Tree ), ( Tree, Tree ) )
same_binary r =
    case r of
        ( Node (Con t1 t2), Node (Con t3 t4) ) ->
            Just ( ( t1, t2 ), ( t3, t4 ) )

        _ ->
            Nothing


different_binary : Redex -> Maybe ( ( Tree -> Tree -> Tree, Tree, Tree ), ( Tree -> Tree -> Tree, Tree, Tree ) )
different_binary r =
    case ( same_binary r, unbinary (first r), unbinary (second r) ) of
        ( Nothing, Just x, Just y ) ->
            Just ( x, y )

        _ ->
            Nothing


contains : String -> Tree -> Bool
contains name tree =
    case ( name, tree ) of
        _ ->
            -- TDDO
            False


i_link : Redex -> Maybe (List Redex)
i_link r =
    -- ?????
    Nothing


i_void : Redex -> Maybe (List Redex)
i_void r =
    if nullary (first r) && nullary (second r) then
        Just []

    else
        Nothing


i_erase : Redex -> Maybe (List Redex)
i_erase r =
    case ( ( nullary (first r), branches (second r) ), ( branches (first r), nullary (second r) ) ) of
        ( ( True, Just ( t1, t2 ) ), _ ) ->
            Just
                [ ( first r, t1 )
                , ( first r, t2 )
                ]

        ( _, ( Just ( t1, t2 ), True ) ) ->
            Just
                [ ( t1, second r )
                , ( t2, second r )
                ]

        _ ->
            Nothing


i_annihilate : Redex -> Maybe (List Redex)
i_annihilate r =
    case same_binary r of
        Just ( ( t1, t2 ), ( t3, t4 ) ) ->
            Just
                [ ( t1, t3 )
                , ( t2, t4 )
                ]

        _ ->
            Nothing


i_commute : Redex -> Maybe (List Redex)
i_commute r =
    case different_binary r of
        Just ( ( f, t1, t2 ), ( g, t3, t4 ) ) ->
            Just
                [ ( g (Var "x") (Var "y"), t1 )
                , ( g (Var "z") (Var "w"), t2 )
                , ( f (Var "x") (Var "z"), t3 )
                , ( f (Var "y") (Var "w"), t4 )
                ]

        _ ->
            Nothing


op : Tree -> Tree -> Tree
op t1 t2 =
    -- TODO
    Node Era


i_operate1 : Redex -> Maybe (List Redex)
i_operate1 r =
    case r of
        ( Node (Ope a n), m ) ->
            Just [ ( a, op n m ) ]

        ( n, Node (Ope m a) ) ->
            Just [ ( op n m, a ) ]

        _ ->
            Nothing


interact : Redex -> List Redex
interact r =
    let
        f i l =
            case i r of
                Just l2 ->
                    l ++ l2

                Nothing ->
                    l
    in
    List.foldr f
        [ r ]
        [ i_link
        , i_void
        , i_erase
        , i_commute
        , i_annihilate
        , i_operate1
        ]

interacts : List Redex -> List Redex
interacts rs =
    case rs of
        r2 :: rs2 ->
            case interact r2 of
                r3 :: rs3 ->
                    if r2 == r3 then
                        r2 :: interacts (rs3 ++ rs2)
                    else
                        interacts (r3 :: rs3 ++ rs2)
                [] ->
                    []
        [] ->
            []


test : Bool
test =
    False


main : Html msg
main =
    text "TODO"
