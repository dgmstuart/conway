module Coordinate exposing (Coordinate, parseCoordinates)

import Regex exposing (replace)


type alias Coordinate =
    ( Int, Int )


parseCoordinates : String -> Result String (List Coordinate)
parseCoordinates input =
    if not (String.isEmpty input) then
        combine (List.map parseCoordinate (String.words input))
    else
        Err "Input was empty"


parseCoordinate : String -> Result String Coordinate
parseCoordinate input =
    buildCoordinate (String.split "," (stripParens input))


{-| Combine a list of results into a single result (holding a list).
taken from <https://github.com/circuithub/elm-result-extra>
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


buildCoordinate : List String -> Result String Coordinate
buildCoordinate list =
    case xCoordinate list of
        Err error ->
            Err error

        Ok xValue ->
            case yCoordinate list of
                Err error ->
                    Err error

                Ok yValue ->
                    Ok ( xValue, yValue )


xCoordinate : List String -> Result String Int
xCoordinate list =
    case List.head list of
        Nothing ->
            Err "No Element"

        Just value ->
            String.toInt value


yCoordinate : List String -> Result String Int
yCoordinate list =
    case List.tail list of
        Nothing ->
            Err "No Element"

        Just tail ->
            case List.head tail of
                Nothing ->
                    Err "No Element"

                Just value ->
                    String.toInt value


stripParens : String -> String
stripParens input =
    Regex.replace Regex.All (Regex.regex "[()]") (\_ -> "") input
