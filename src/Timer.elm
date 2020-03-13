module Timer exposing (Timer, actionTimerEncode, timerDataDecoder, timerDecoder)

import Json.Decode as JD
import Json.Encode as JE


type alias Timer =
    { idTimer : Int
    , idCapture : Int
    , startedAt : String
    , stoppedAt : Maybe String
    }


timerDataDecoder : JD.Decoder Timer
timerDataDecoder =
    JD.field "data" timerDecoder


timerDecoder : JD.Decoder Timer
timerDecoder =
    JD.map4 Timer
        (JD.field "timer_id" JD.int)
        (JD.field "capture_id" JD.int)
        (JD.field "started_at" JD.string)
        (JD.maybe (JD.field "stopped_at" JD.string))


actionTimerEncode : String -> JD.Value
actionTimerEncode action =
    JE.object
        [ ( "action", JE.string action ) ]
