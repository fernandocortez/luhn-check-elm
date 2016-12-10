module LuhnCheck exposing (luhnCheck)

import String exposing (length, reverse, split, toInt)
import Result exposing (withDefault)
import List exposing (map, drop, foldr)
import Util exposing (removeNonDigits)


{-
   * Check Digit Scheme
   * http://www.cybersource.com/developers/getting_started/test_and_manage/best_practices/check_digit_scheme/
   *
   * A check digit is a digit added to a number (either at the end or the
   * beginning) that validates the authenticity of the number. A simple algorithm
   * is applied to the other digits of the number which yields the check digit.
   * Check Digit Scheme can be used when an end-user has entered in a credit card
   * number and you want to validate it before sending it an authorization.
   *
   * This self-checking scheme (referred to as the Luhn Mod-10 Method) is an
   * international standard for validating card account numbers
   * (ISO 2894/ANSI 4.13). Such account numbers, which cannot exceed 19 digits
   * including the check digit, are assigned, embossed and encoded to include a
   * single check digit in the rightmost position. The check digit is calculated
   * as follows:
   *
   *   1. Beginning on the right with the digit which immediately precedes the
   *      check digit and moving toward the left, double every other digit. After
   *      doubling each selected digit, if the result is ten or greater, add the
   *      two digits together to arrive at a single-digit result.
   *   2. Each individual resulting digit (plus those skipped above) are then
   *      added together.
   *   3. This sum is then subtracted from the lowest multiple of ten which is
   *      equal to or greater than the sum and the single-digit result is the
   *      check digit.
   *
-}


luhnCheck : String -> Bool
luhnCheck creditCardNumber =
    let
        pan =
            removeNonDigits creditCardNumber
    in
        if length pan < 13 then
            False
        else if length pan > 19 then
            False
        else
            pan
                |> String.reverse
                |> String.split ""
                |> convertToListOfInts
                |> everySecondDigitDoubled
                |> sumOfDigitsIfGreaterThanTen
                |> sum_
                |> (\n -> n % 10 == 0)


sum_ : List Int -> Int
sum_ digits =
    foldr (+) 0 digits


sumOfDigitsIfGreaterThanTen : List Int -> List Int
sumOfDigitsIfGreaterThanTen digits =
    {- this is an arithmetic trick in place of adding the digits that make
       up the number. For example adding the digits of 10 (ten) would be
       1 + 0 = 1, which is the same as 10 - 9 = 1
    -}
    map
        (\n ->
            if n >= 10 then
                n - 9
            else
                n
        )
        digits


everySecondDigitDoubled : List Int -> List Int
everySecondDigitDoubled digits =
    case digits of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: _ ->
            x :: 2 * y :: everySecondDigitDoubled (drop 2 digits)


convertToListOfInts : List String -> List Int
convertToListOfInts digits =
    -- withDefault is just to compile. Non digit is not expected
    map (\d -> d |> toInt |> withDefault 0) digits
