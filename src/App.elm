module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Location = (Int, Int)

type Marked
    = Blank Location
    | X
    | O

type Turn
    = XTurn
    | OTurn

type alias Row =
    List Marked


type alias Model =
    {
        board : List Row
        , turn : Turn
    }


init : ( Model, Cmd Msg )
init =
    (
        {
            board = [
           [Blank (0,0), Blank (0,1), Blank (0,2)]
           , [Blank (1,0), Blank (1,1), Blank (1,2)]
           , [Blank (2,0), Blank (2,1), Blank (2,2)]
       ], turn = XTurn
        }
       
    , Cmd.none
    )



-- UPDATE


type Msg
    = Play (Int, Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Play lokey ->
            let updatedBoard = 
                ( List.map (List.map (idk2 lokey model.turn) ) model.board )
                newTurn = 
                    case model.turn of
                        XTurn ->
                            OTurn
                        OTurn ->
                            XTurn

            in
                ( { turn = newTurn, board = updatedBoard }, Cmd.none )


idk2 : Location -> Turn -> Marked -> Marked
idk2 lokey turny marky = 
    case marky of
        Blank cellLokey ->
            if cellLokey == lokey then
                case turny of
                    XTurn ->
                        X
                    OTurn -> 
                        O
            else
                Blank cellLokey
        X ->
            X
        O ->
            O


-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map oneRow model.board)


oneRow : Row -> Html Msg
oneRow hmm = List.map viewMarked hmm |> (\x -> div [] x)


viewMarked : Marked -> Html Msg
viewMarked marked =
    case marked of
        Blank (a,b) ->
            button [onClick (Play (a,b) ) ] [text " Blanco "]

        X ->
            text " __X__ "

        O ->
            text " __O__ "

