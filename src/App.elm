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
    , won: Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { board =
            [ [ Blank ( 0, 0 ), Blank ( 0, 1 ), Blank ( 0, 2 ) ]
            , [ Blank ( 1, 0 ), Blank ( 1, 1 ), Blank ( 1, 2 ) ]
            , [ Blank ( 2, 0 ), Blank ( 2, 1 ), Blank ( 2, 2 ) ]
            ]
      , turn = XTurn
      , won = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Play ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Play lokey ->
            let
                updatedBoard =
                    (List.map (List.map (idk2 lokey model.turn)) model.board)

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
                    ( { model | won = True, board = updatedBoard}, Cmd.none )
                OWon ->
                    ( { model | won = True, board = updatedBoard}, Cmd.none )

checkWinner : Board -> GameStatus
checkWinner boardy =
    let
        flatAndNumbedBoard = 
            ( List.indexedMap (,) (List.foldr (++) [] boardy) )
        xes =
            List.filterMap xflattener flatAndNumbedBoard
    in
        if bigBoyChecker xes winnerWinners then
            XWon
        else
            Playing


bigBoyChecker : List Int -> List (List Int) -> Bool
bigBoyChecker playIdxs winTruth =
    List.any (firstListMembersOfSecond playIdxs) winTruth

-- is all of a in b ?
firstListMembersOfSecond : List Int -> List Int -> Bool
firstListMembersOfSecond b a =
    List.all (listMemberSwitch b) a

listMemberSwitch : List a -> a -> Bool
listMemberSwitch collection val =
    List.member val collection




xflattener : ( Int, Marked ) -> Maybe Int
xflattener = keepCertainIdxs X

oflattener : ( Int, Marked ) -> Maybe Int
oflattener = keepCertainIdxs O

keepCertainIdxs : Marked -> (Int, Marked) -> Maybe Int
keepCertainIdxs whichTeam (idx, sup) =
    if sup == whichTeam then
        Just idx
    else
        Nothing

winnerWinners : List (List Int)
winnerWinners = 
    [
        [0,3,6]
        ,[1,4,7]
        ,[2,5,8]
        ,[0,1,2]
        ,[3,4,5]
        ,[6,7,8]
        ,[0,4,8]
        ,[2,4,6]
    ]


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
    if model.won then
        text "someone won!"
    else
        div [] (List.map oneRow model.board)


oneRow : Row -> Html Msg
oneRow hmm =
    List.map viewMarked hmm |> (\x -> div [class "row"] x)


viewMarked : Marked -> Html Msg
viewMarked marked =
    case marked of
        Blank ( a, b ) ->
            div [ class "cell", onClick (Play ( a, b )) ] []

        X ->
            div [class "cell"] [text "X"]

        O ->
            div [class "cell"] [text "O"]
