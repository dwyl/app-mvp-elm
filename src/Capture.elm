module Capture exposing (Capture, CaptureStatus(..), captureDataDecoder, captureDecoder, captureEncode, captureStatusDecoder, capturesDataDecoder, completedToStatusDecoder, getCurrentTimer, getPreviousTimer, initCapture, savedCaptureDecoder, timersToStatusDecoder)

import Json.Decode as JD
import Json.Encode as JE
import Timer exposing (..)


type alias Capture =
    { idCapture : Int
    , text : String
    , timers : List Timer
    , status : CaptureStatus
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
    JD.map4 Capture
        (JD.field "capture_id" JD.int)
        (JD.field "text" JD.string)
        (JD.field "timers" (JD.list timerDecoder))
        (JD.field "completed" JD.bool |> JD.andThen captureStatusDecoder)


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
        [ ( "text", JE.string capture.text ) ]


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
