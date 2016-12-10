module Util exposing (formatPrimaryAccountNumber, removeNonDigits)

import String exposing (slice)
import Regex exposing (contains, replace, regex, HowMany(All))


formatPrimaryAccountNumber : String -> String
formatPrimaryAccountNumber pan =
    let
        sanitizedPan =
            removeNonDigits pan
    in
        if sanitizedPan |> isAmex then
            sanitizedPan
                |> slice 0 15
        else
            sanitizedPan
                |> slice 0 16


removeNonDigits : String -> String
removeNonDigits =
    replace All (regex "\\D") (\_ -> "")


isAmex : String -> Bool
isAmex pan =
    pan |> contains (regex "^3[47]")
