module Common exposing (..)

import Process
import Task


notifyIn : msg -> Float -> Cmd msg
notifyIn msg time =
    Process.sleep time
        |> Task.attempt (\_ -> msg)
