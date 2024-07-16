module Env exposing (..)


type alias Env a =
    List ( String, a )


lookup : String -> Env a -> Maybe a
lookup name env =
    env
        |> List.filterMap
            (\( n, v ) ->
                if n == name then
                    Just v

                else
                    Nothing
            )
        |> List.head


extend : String -> a -> Env a -> Env a
extend name value env =
    ( name, value ) :: env
