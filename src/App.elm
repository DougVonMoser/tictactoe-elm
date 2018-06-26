module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Marked = Blank | X | O

type alias Model =
    List Marked
    -- List (List Marked)


init : ( Model, Cmd Msg )
init =
    ( 
        -- [
        [Blank, Blank, Blank]
        -- , [Blank, Blank, Blank]
        -- , [Blank, Blank, Blank]
    -- ]
    , Cmd.none )



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
    div [] (List.map viewMarked model)



viewMarked : Marked -> Html Msg
viewMarked marked =
    case marked of
        Blank ->
            text "Blanco"
        X ->
            text "X"
        O -> 
            text "O"