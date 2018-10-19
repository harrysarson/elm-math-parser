module Config exposing (backgroundColors, outputPaddingX, textColors, textSize)

import Element as E


backgroundColors =
    { stage = E.rgba 0.2 0 0 1
    , input = E.rgba 1 0.8 0.8 1
    , error = E.rgba 0.8 0.8 1 1
    }


textColors =
    { input = E.rgba 0.1 0.1 0.5 1
    , output = E.rgba 0.7 0.7 1 1
    , error = backgroundColors.stage
    }


outputPaddingX =
    12


textSize =
    16
