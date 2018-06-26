module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Marked
    = Blank
    | X
    | O


type alias Row =
    List Marked


type alias Model =
    List Row


init : ( Model, Cmd Msg )
init =
    ( [ [ Blank, Blank, Blank ]
      , [ Blank, Blank, Blank ]
      , [ Blank, Blank, Blank ]
      ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Inc


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map oneRow model)


oneRow : Row -> Html Msg
oneRow hmmm =
    List.map viewMarked hmmm |> (\x -> div [] x)


viewMarked : Marked -> Html Msg
viewMarked marked =
    case marked of
        Blank ->
            text "Blanco"

        X ->
            text "__X__"

        O ->
            text "__O__"

