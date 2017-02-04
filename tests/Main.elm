port module Main exposing (main, emit)

import Test.Runner.Node exposing (run, TestProgram)
import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (Fuzzer, intRange)
import Expect
import Json.Encode exposing (Value)
import Dict
import Dict.Extra exposing (..)
import Set


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg


tests : Test
tests =
    describe "Dict tests"
        [ groupByTests
        , fromListByTests
        , removeWhenTests
        , removeManyTests
        , keepOnlyTests
        , mapKeysTests
        ]



-- groupBy


groupByTests : Test
groupByTests =
    describe "groupBy"
        [ test "example" <|
            \() ->
                Dict.toList (groupBy .id [ mary, jack, jill ])
                    |> Expect.equal [ ( 1, [ mary, jill ] ), ( 2, [ jack ] ) ]
        ]


type alias GroupByData =
    { id : Int
    , name : String
    }


mary : GroupByData
mary =
    GroupByData 1 "Mary"


jack : GroupByData
jack =
    GroupByData 2 "Jack"


jill : GroupByData
jill =
    GroupByData 1 "Jill"



-- fromListBy


fromListByTests : Test
fromListByTests =
    describe "fromListBy"
        [ test "example" <|
            \() ->
                fromListBy .id [ jack, jill ]
                    |> Expect.equal (Dict.fromList [ ( 2, jack ), ( 1, jill ) ])
        , test "replacement" <|
            \() ->
                fromListBy .id [ jack, jill, mary ]
                    |> Expect.equal (Dict.fromList [ ( 2, jack ), ( 1, mary ) ])
        ]



-- removeWhen


removeWhenTests : Test
removeWhenTests =
    describe "removeWhen"
        [ test "example" <|
            \() ->
                removeWhen (\_ v -> v == 1) (Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ])
                    |> Expect.equal (Dict.fromList [ ( "Jack", 2 ) ])
        ]



-- removeMany


removeManyTests : Test
removeManyTests =
    describe "removeMany"
        [ test "example" <|
            \() ->
                removeMany (Set.fromList [ "Mary", "Jill" ]) (Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ])
                    |> Expect.equal (Dict.fromList [ ( "Jack", 2 ) ])
        ]



-- keepOnly


keepOnlyTests : Test
keepOnlyTests =
    describe "keepOnly"
        [ test "example" <|
            \() ->
                keepOnly (Set.fromList [ "Jack", "Jill" ]) (Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ])
                    |> Expect.equal (Dict.fromList [ ( "Jack", 2 ), ( "Jill", 1 ) ])
        ]



-- mapKeys


mapKeysTests : Test
mapKeysTests =
    describe "mapKeys"
        [ test "example" <|
            \() ->
                mapKeys ((+) 1) (Dict.fromList [ ( 1, "Jack" ), ( 2, "Jill" ) ])
                    |> Expect.equal (Dict.fromList [ ( 2, "Jack" ), ( 3, "Jill" ) ])
        ]
