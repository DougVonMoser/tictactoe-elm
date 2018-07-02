module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Location =
    ( Int, Int )


type GameStatus
    = Playing
    | XWon
    | OWon
    | CatsGame


type Marked
    = Blank Location
    | X
    | O


type Turn
    = XTurn
    | OTurn


type alias Row =
    List Marked


type alias Board =
    List Row


type alias Model =
    { board : Board
    , turn : Turn
    , won : GameStatus
    }


winnerWinners : List (List Int)
winnerWinners =
    [ [ 0, 3, 6 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 0, 1, 2 ]
    , [ 3, 4, 5 ]
    , [ 6, 7, 8 ]
    , [ 0, 4, 8 ]
    , [ 2, 4, 6 ]
    ]


init : ( Model, Cmd Msg )
init =
    ( { board =
            [ [ Blank ( 0, 0 ), Blank ( 0, 1 ), Blank ( 0, 2 ) ]
            , [ Blank ( 1, 0 ), Blank ( 1, 1 ), Blank ( 1, 2 ) ]
            , [ Blank ( 2, 0 ), Blank ( 2, 1 ), Blank ( 2, 2 ) ]
            ]
      , turn = XTurn
      , won = Playing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Play ( Int, Int )
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Play lokey ->
            let
                updatedBoard =
                    List.map (List.map (idk2 lokey model.turn)) model.board

                newTurn =
                    case model.turn of
                        XTurn ->
                            OTurn

                        OTurn ->
                            XTurn
            in
                case checkWinner updatedBoard of
                    Playing ->
                        ( { model | turn = newTurn, board = updatedBoard }, Cmd.none )

                    XWon ->
                        ( { model | won = XWon, board = updatedBoard }, Cmd.none )

                    OWon ->
                        ( { model | won = OWon, board = updatedBoard }, Cmd.none )

                    CatsGame ->
                        ( { model | won = CatsGame, board = updatedBoard }, Cmd.none )


        Reset ->
            init


checkWinner : Board -> GameStatus
checkWinner boardy =
    let
        flatAndNumbedBoard =
            List.indexedMap (,) (List.foldr (++) [] boardy)

        xes =
            List.filterMap xflattener flatAndNumbedBoard

        oes =
            List.filterMap oflattener flatAndNumbedBoard
    in
        if bigBoyChecker xes winnerWinners then
            XWon

        -- this can be more robust
        else if List.length xes == 5 then
            CatsGame
        else if bigBoyChecker oes winnerWinners then
            OWon
        else
            Playing


bigBoyChecker : List Int -> List (List Int) -> Bool
bigBoyChecker playIdxs =
    List.any (firstListMembersOfSecond playIdxs)


firstListMembersOfSecond : List Int -> List Int -> Bool
firstListMembersOfSecond b =
    List.all (listMemberSwitch b)


listMemberSwitch : List a -> a -> Bool
listMemberSwitch collection val =
    List.member val collection


xflattener : ( Int, Marked ) -> Maybe Int
xflattener =
    keepCertainIdxs X


oflattener : ( Int, Marked ) -> Maybe Int
oflattener =
    keepCertainIdxs O


keepCertainIdxs : Marked -> ( Int, Marked ) -> Maybe Int
keepCertainIdxs whichTeam ( idx, sup ) =
    if sup == whichTeam then
        Just idx
    else
        Nothing


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

        _ ->
            marky



-- VIEW


view : Model -> Html Msg
view model =
    case model.won of
        Playing ->
            div [] (List.map oneRow model.board)

        _ ->
            div []
                [ winText model.won
                , button [ onClick Reset ] [ text "reset!" ]
                ]


winText : GameStatus -> Html msg
winText whowon =
    case whowon of
        XWon ->
            text "X won, yay! "
        OWon ->
            text "O won, yay! "
        _ ->
            text "cats game you dumb dumbs "


oneRow : Row -> Html Msg
oneRow hmm =
    List.map viewMarked hmm |> (\x -> div [ class "row" ] x)


viewMarked : Marked -> Html Msg
viewMarked marked =
    case marked of
        Blank ( a, b ) ->
            div [ class "cell", onClick (Play ( a, b )) ] []

        X ->
            div [ class "cell" ] [ text "X" ]

        O ->
            div [ class "cell" ] [ text "O" ]
