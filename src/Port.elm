port module Port exposing (receive, send)

import Json.Decode as JD


port send : JD.Value -> Cmd msg


port receive : (JD.Value -> msg) -> Sub msg
