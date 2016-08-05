module Dict.Extra exposing (groupBy, fromListBy, removeWhen, removeMany, keepOnly)

{-| Convenience functions for working with `Dict`

# List operations
@docs groupBy, fromListBy

# Manipulation
@docs removeWhen, removeMany, keepOnly
-}

import Dict exposing (Dict)
import Set exposing (Set)

{-| Takes a key-fn and a list, creates a `Dict` which maps the key returned from key-fn, to a list of matching elements.

    mary = {groupId: 1, name: "Mary"}
    jack = {groupId: 2, name: "Jack"}
    jill = {groupId: 1, name: "Jill"}
    groupBy .groupId [mary, jack, jill] == Dict.fromList [(1, [jill, mary]), (2, [jack])]
-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy keyfn list =
    List.foldl
        (\x acc ->
            let
                key =
                    keyfn x
            in
                Dict.update key (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        Dict.empty
        list


{-| Create a dictionary from a list of values, by passing a function that can get a key from any such value. This can, for instance, be useful when constructing Dicts from a List of records with `id` fields:

    fromListBy .id [{ id=1, name="Jack" }, { id=2, name="Jill" }] == Dict.fromList [(1, { id=1, name="Jack" }), (2, { id=2, name="Jill" })]
-}
fromListBy : (a -> comparable) -> List a -> Dict comparable a
fromListBy keyfn xs =
    List.foldl (\x acc -> Dict.insert (keyfn x) x acc) Dict.empty xs


{-| Keep elements which fail to satisfy the predicate.
    This is functionally equivalent to `Dict.filter (not << predicate) dict`.

    removeWhen (\c v -> v == 1) Dict.fromList [("Mary", 1), ("Jack", 2), ("Jill", 1)] == Dict.fromList [("Jack", 2)]
-}
removeWhen : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
removeWhen pred dict =
    Dict.filter (\c v -> not (pred c v)) dict


{-| Keep a key-value pair exactly if its key does not appear in the set.
-}
removeMany : Set comparable -> Dict comparable v -> Dict comparable v
removeMany set dict =
    Set.foldl Dict.remove dict set


{-| Keep a key-value pair exactly if its key appears in the set.
-}
keepOnly : Set comparable -> Dict comparable v -> Dict comparable v
keepOnly set dict =
    Set.foldl (\k acc -> Maybe.withDefault acc (Maybe.map (\v -> Dict.insert k v acc) (Dict.get k dict))) Dict.empty set
