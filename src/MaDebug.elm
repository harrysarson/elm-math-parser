module MaDebug exposing (log)

{-| Allows logging to be enabled/disabled.

@docs log

-}


enableLogging : Bool
enableLogging =
    False


{-| log a value, similar to `Debug.log` but can be disabled.
-}
log : String -> a -> a
log =
    if enableLogging then
        Debug.log
    else
        always identity
