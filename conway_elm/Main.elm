module Main exposing (..)

import Html exposing (Html, button, div, p, pre, text, input, program)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Coordinate exposing (Coordinate, parseCoordinates)


-- MODEL


type alias Model =
    { coordinates : List Coordinate
    , input : String
    , errors : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel =
    { coordinates = []
    , input = ""
    , errors = Nothing
    }



-- UPDATE


type Msg
    = Reset
    | Load String
    | Set
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( emptyModel, Cmd.none )

        Load input ->
            ( { model | input = input }, Cmd.none )

        Set ->
            case parseCoordinates model.input of
                Ok coordinates ->
                    ( { model | errors = Nothing, coordinates = coordinates }, Cmd.none )

                Err errors ->
                    ( { model | errors = Just errors }, Cmd.none )

        Step ->
            ( model, Cmd.none )


getCoordinates : String -> Result String (List Coordinate)
getCoordinates input =
    if not (String.isEmpty input) then
        parseCoordinates input
    else
        Err "Please enter some starting coordinates"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] (maybeToHtml model.errors)
        , input [ placeholder "e.g. (1,2) (3,4)", onInput Load ] []
        , button [ onClick Set ] [ text "Load" ]
        , pre []
            [ text (toString model.coordinates) ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Step ] [ text "Step" ]
        ]


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
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }