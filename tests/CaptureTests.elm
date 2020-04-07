module CaptureTests exposing (suite)

import Capture exposing (Capture, CaptureStatus(..), compareCaptures)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Pages.Capture exposing (SortCaptures(..))
import Test exposing (..)


capturesTest : CaptureStatus -> CaptureStatus -> ( Capture, Capture )
capturesTest status1 status2 =
    let
        capture1 =
            { idCapture = 1
            , text = ""
            , timers = []
            , status = status1
            }

        capture2 =
            { idCapture = 2
            , text = ""
            , timers = []
            , status = status2
            }
    in
    ( capture1, capture2 )


suite : Test
suite =
    describe "sort captures by status"
        [ test "InProgress: todo GT than inProgress" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest ToDo InProgress
                in
                compareCaptures InProgress capture1 capture2
                    |> Expect.equal GT
        , test "inPorgress: inProgress LT todo" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest InProgress ToDo
                in
                compareCaptures InProgress capture1 capture2
                    |> Expect.equal LT
        , test "inPorgress: inProgress EQ InProgress" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest InProgress InProgress
                in
                compareCaptures InProgress capture1 capture2
                    |> Expect.equal EQ
        , test "ToDo: inProgress GT ToDo" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest InProgress ToDo
                in
                compareCaptures ToDo capture1 capture2
                    |> Expect.equal GT
        , test "ToDo: ToDo GT InProgress" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest ToDo InProgress
                in
                compareCaptures ToDo capture1 capture2
                    |> Expect.equal LT
        , test "ToDo: ToDo EQ ToDo" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest ToDo ToDo
                in
                compareCaptures ToDo capture1 capture2
                    |> Expect.equal EQ
        , test "Completed: ToDo GT Completed" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest ToDo Completed
                in
                compareCaptures Completed capture1 capture2
                    |> Expect.equal GT
        , test "Completed: Completed LT ToDo" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest Completed ToDo
                in
                compareCaptures Completed capture1 capture2
                    |> Expect.equal LT
        , test "Completed: Completed EQ Completed" <|
            \_ ->
                let
                    ( capture1, capture2 ) =
                        capturesTest Completed Completed
                in
                compareCaptures Completed capture1 capture2
                    |> Expect.equal EQ
        ]
