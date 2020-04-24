module Capture exposing (Capture, CaptureStatus(..), captureDataDecoder, captureDecoder, captureEncode, captureStatusDecoder, captureStatusToString, capturesDataDecoder, compareCaptures, completedToStatusDecoder, getCurrentTimer, getPreviousTimer, initCapture, savedCaptureDecoder, timersToStatusDecoder)

import Json.Decode as JD
import Json.Encode as JE
import Timer exposing (..)


type alias Capture =
    { idCapture : Int
    , text : String
    , timers : List Timer
    , status : CaptureStatus
    , tags : List String
    }


type CaptureStatus
    = ToDo
    | Completed
    | InProgress
    | Disabled
    | Error String


initCapture : Capture
initCapture =
    { idCapture = 0
    , text = ""
    , timers = []
    , status = ToDo
    , tags = []
    }


capturesDataDecoder : JD.Decoder (List Capture)
capturesDataDecoder =
    JD.field "data" (JD.list captureDecoder)


captureDataDecoder : JD.Decoder Capture
captureDataDecoder =
    JD.field "data" captureDecoder


savedCaptureDecoder : JD.Decoder Capture
savedCaptureDecoder =
    JD.field "data" captureDecoder


captureDecoder : JD.Decoder Capture
captureDecoder =
    JD.map5 Capture
        (JD.field "capture_id" JD.int)
        (JD.field "text" JD.string)
        (JD.field "timers" (JD.list timerDecoder))
        (JD.field "completed" JD.bool |> JD.andThen captureStatusDecoder)
        (JD.field "tags" (JD.list (JD.field "text" JD.string)))


captureStatusDecoder : Bool -> JD.Decoder CaptureStatus
captureStatusDecoder completed =
    if completed then
        completedToStatusDecoder

    else
        timersToStatusDecoder


completedToStatusDecoder : JD.Decoder CaptureStatus
completedToStatusDecoder =
    JD.succeed Completed


timersToStatusDecoder : JD.Decoder CaptureStatus
timersToStatusDecoder =
    JD.field "timers" (JD.list timerDecoder)
        |> JD.andThen
            (\timers ->
                case List.head timers of
                    Nothing ->
                        JD.succeed ToDo

                    Just timer ->
                        case timer.stoppedAt of
                            Nothing ->
                                JD.succeed InProgress

                            _ ->
                                JD.succeed ToDo
            )


captureEncode : Capture -> JD.Value
captureEncode capture =
    JE.object
        [ ( "text", JE.string capture.text )
        , ( "completed", JE.bool (capture.status == Completed) )
        , ( "tags", JE.string (String.join ", " capture.tags) )
        ]


{-| Return the current timer linked to the capture
As the API returns the list of timer in descending order from
when they were created, the current timer is the head of the list of timers
-}
getCurrentTimer : List Timer -> Maybe Timer
getCurrentTimer timers =
    List.head timers


getPreviousTimer : List Timer -> Maybe (List Timer)
getPreviousTimer timers =
    List.tail timers


captureStatusToString : CaptureStatus -> String
captureStatusToString status =
    case status of
        ToDo ->
            "start"

        Completed ->
            "completed"

        InProgress ->
            "stop"

        _ ->
            ""


compareCaptures : CaptureStatus -> Capture -> Capture -> Order
compareCaptures sortBy c1 c2 =
    case ( sortBy, c1.status, c2.status ) of
        ( InProgress, InProgress, InProgress ) ->
            EQ

        ( InProgress, _, InProgress ) ->
            GT

        ( InProgress, InProgress, _ ) ->
            LT

        ( ToDo, ToDo, ToDo ) ->
            EQ

        ( ToDo, _, ToDo ) ->
            GT

        ( ToDo, _, _ ) ->
            LT

        ( Completed, Completed, Completed ) ->
            EQ

        ( Completed, _, Completed ) ->
            GT

        ( Completed, _, _ ) ->
            LT

        _ ->
            EQ
