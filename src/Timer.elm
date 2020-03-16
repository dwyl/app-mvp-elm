module Timer exposing (Timer, actionTimerEncode, timerDataDecoder, timerDecoder)

import Json.Decode as JD
import Json.Encode as JE
import Time
import Iso8601

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
