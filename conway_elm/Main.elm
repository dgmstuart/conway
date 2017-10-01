module Main exposing (..)

import Html exposing (Html, button, div, p, pre, text, input, beginnerProgram)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Coordinate exposing (Coordinate, parseCoordinates)
import Set exposing (Set)


-- MODEL


type alias Model =
    { livingCells : LivingCells
    , input : String
    , errors : Maybe String
    }


model : Model
model =
    emptyModel


emptyModel =
    { livingCells = Set.empty
    , input = ""
    , errors = Nothing
    }



-- UPDATE


type Msg
    = Reset
    | Load String
    | Set
    | Step


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            emptyModel

        Load input ->
            { model | input = input }

        Set ->
            case parseCoordinates model.input of
                Ok livingCells ->
                    { model | errors = Nothing, livingCells = livingCells }

                Err errors ->
                    { model | errors = Just errors }

        Step ->
            { model | livingCells = (next model.livingCells) }


getlivingCells : String -> Result String LivingCells
getlivingCells input =
    if not (String.isEmpty input) then
        parseCoordinates input
    else
        Err "Please enter some starting coordinates"


next : LivingCells -> LivingCells
next livingCells =
    Set.foldr
        (updateLiving (decide livingCells))
        Set.empty
        (neighbours livingCells)


updateLiving : (Coordinate -> Bool) -> Coordinate -> LivingCells -> LivingCells
updateLiving f cell newLivingCells =
    if (f cell) then
        (Set.insert cell newLivingCells)
    else
        newLivingCells


type alias Neighbours =
    Set Coordinate


neighbours : LivingCells -> Neighbours
neighbours livingCells =
    Set.foldr (addNeighbours) livingCells livingCells


addNeighbours : Coordinate -> Neighbours -> Neighbours
addNeighbours cell neighbours =
    Set.union (eightNeighbours cell) neighbours


eightNeighbours : Coordinate -> Set Coordinate
eightNeighbours ( x, y ) =
    let
        xs =
            List.range (x - 1) (x + 1)

        ys =
            List.range (y - 1) (y + 1)
    in
        Set.remove ( x, y ) (Set.fromList (List.concat (List.map (\f -> List.map f xs) (List.map (\y x -> ( x, y )) ys))))


decide : LivingCells -> Coordinate -> Bool
decide livingCells cell =
    let
        livingNeighbours =
            livingNeighbourCount cell livingCells
    in
        if Set.member cell livingCells then
            case livingNeighbours of
                2 ->
                    True

                3 ->
                    True

                _ ->
                    False
        else
            case livingNeighbours of
                3 ->
                    True

                _ ->
                    False


livingNeighbourCount : Coordinate -> LivingCells -> Int
livingNeighbourCount cell livingCells =
    Set.size (Set.intersect (eightNeighbours cell) livingCells)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] (maybeToHtml model.errors)
        , input [ placeholder "e.g. (1,2) (3,4)", onInput Load ] []
        , button [ onClick Set ] [ text "Load" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Step ] [ text "Step" ]
        , pre []
            [ text (coordinatesToString model.livingCells) ]
        ]


coordinatesToString : LivingCells -> String
coordinatesToString livingCells =
    String.join "\n" (List.map (List.foldr (String.cons) "") (livingCellsToStates livingCells))


type alias LivingCells =
    Set Coordinate


type alias WorldBoundaries =
    ( Coordinate, Coordinate )


livingCellsToStates : LivingCells -> List (List Char)
livingCellsToStates livingCells =
    List.map (List.map (showState livingCells)) (worldFromCoordinates livingCells)


worldFromCoordinates : Set Coordinate -> List (List Coordinate)
worldFromCoordinates coordinates =
    buildWorld (worldBoundaries coordinates)


buildWorld : WorldBoundaries -> List (List Coordinate)
buildWorld ( ( minX, minY ), ( maxX, maxY ) ) =
    let
        xs =
            List.range (minX - 1) (maxX + 1)

        ys =
            List.range (minY - 1) (maxY + 1)
    in
        List.map (\f -> List.map f xs) (List.map (\y x -> ( x, y )) ys)


worldBoundaries : Set Coordinate -> WorldBoundaries
worldBoundaries coordinates =
    let
        origin =
            ( ( 0, 0 ), ( 0, 0 ) )
    in
        Set.foldr minMaxCoords origin coordinates


minMaxCoords : Coordinate -> WorldBoundaries -> WorldBoundaries
minMaxCoords ( x, y ) ( ( minX, minY ), ( maxX, maxY ) ) =
    ( ( (min minX x), (min minY y) )
    , ( (max maxX x), (max maxY y) )
    )


showState : LivingCells -> Coordinate -> Char
showState coordinates coordinate =
    if Set.member coordinate coordinates then
        'x'
    else
        '.'


{-| Adapted from <https://github.com/circuithub/elm-maybe-extra>
-}
maybeToHtml : Maybe String -> List (Html Msg)
maybeToHtml m =
    case m of
        Nothing ->
            []

        Just x ->
            [ text x ]



-- MAIN


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
