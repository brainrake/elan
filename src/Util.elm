module Util exposing (..)


fix : (a -> a) -> a -> a
fix f x =
    let
        result =
            f x
    in
    if result == x then
        result

    else
        fix f result
