module Timer exposing (Timer, actionTimerEncode, millisToHMS, timerDataDecoder, timerDecoder, timerToMillis, timersToMillis)

import Iso8601
import Json.Decode as JD
import Json.Encode as JE
import Time


type alias Timer =
    { idTimer : Int
    , idCapture : Int
    , startedAt : Time.Posix
    , stoppedAt : Maybe Time.Posix
    }


timerDataDecoder : JD.Decoder Timer
timerDataDecoder =
    JD.field "data" timerDecoder


timerDecoder : JD.Decoder Timer
timerDecoder =
    JD.map4 Timer
        (JD.field "timer_id" JD.int)
        (JD.field "capture_id" JD.int)
        (JD.field "started_at" Iso8601.decoder)
        (JD.maybe (JD.field "stopped_at" Iso8601.decoder))


actionTimerEncode : String -> JD.Value
actionTimerEncode action =
    JE.object
        [ ( "action", JE.string action ) ]



{- Calculate number of millisecond between startedAt and stoppedAt
   The returned value is of type Maybe.
   Nothing is returned if the stoppedAt value is not defined yet, ie Nothing.
-}

startedAtToMillis : Timer -> Int
startedAtToMillis timer =
    Time.posixToMillis timer.startedAt

timerToMillis : Timer -> Maybe Int
timerToMillis timer =
    Maybe.map2
        (\p1 p2 -> Time.posixToMillis p1 - Time.posixToMillis p2)
        timer.stoppedAt
        (Just timer.startedAt)



{- Sum all the timers in milliseconds.
   Returns Nothing if one of the timer is still running
   otherwise Just the timing
-}


timersToMillis : List Timer -> Maybe Int
timersToMillis timers =
    let
        millis =
            List.map timerToMillis timers
    in
    List.foldl (\a b -> Maybe.map2 (+) a b) (Just 0) millis

millisToHMS : Int -> ( String, String, String )
millisToHMS millis =
    let
        hour =
            formatTime <| modBy 24 (millis // (1000 * 60 * 60))

        minute =
            formatTime <| modBy 60 (millis // (1000 * 60))

        second =
            formatTime <| modBy 60 (millis // 1000)
    in
    ( hour, minute, second )



{- Format an integer to a string by prepending '0'
   if the string length < 2
-}


formatTime : Int -> String
formatTime =
    String.padLeft 2 '0' << String.fromInt
