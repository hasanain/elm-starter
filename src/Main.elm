module Main exposing (..)

import Html exposing (beginnerProgram, div, button, text, Html, h2)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Array exposing (Array, initialize, get, set, map, toList, indexedMap)


main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Board =
    Array (Array Int)


type alias GameState =
    { board : Board
    , player1 : Bool
    }


model : GameState
model =
    getInitialModel


getInitialModel : GameState
getInitialModel =
    { board = initialize 3 (\_ -> (initialize 3 (always 0)))
    , player1 = True
    }


makeMove : GameState -> Int -> Int -> GameState
makeMove state r c =
    let
        maybeRow =
            get r state.board

        p1 =
            state.player1

        result =
            gameResult state.board
    in
        if result == 0 then
            case maybeRow of
                Just row ->
                    { state
                        | player1 = not p1
                        , board =
                            set r
                                (set c
                                    (if p1 then
                                        2
                                     else
                                        1
                                    )
                                    row
                                )
                                state.board
                    }

                Nothing ->
                    state
        else
            state


winnerHorizontal : List (List Int) -> Bool
winnerHorizontal board =
    let
        summedBoard =
            List.map (List.sum) board

        emptyTiles =
            List.map (\l -> not (List.member 0 l)) board
    in
        List.foldl (\( n, v ) a -> a || (v && (n == 6 || n == 3))) False <| List.map2 (,) summedBoard emptyTiles


transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
                (x :: heads) :: transpose (xs :: tails)


winnerVertical : List (List Int) -> Bool
winnerVertical board =
    let
        transposedBoard =
            transpose board
    in
        winnerHorizontal transposedBoard


getDiagonalElem : Int -> List Int -> Int
getDiagonalElem i l =
    let
        xs =
            List.drop i l

        maybeElem =
            List.head xs
    in
        case maybeElem of
            Just elem ->
                elem

            Nothing ->
                0


getDiagonal : List (List Int) -> List Int
getDiagonal ll =
    List.indexedMap getDiagonalElem ll


winnerDiagonal : List (List Int) -> Bool
winnerDiagonal board =
    let
        diagonal1 =
            getDiagonal board

        diagonal2 =
            getDiagonal <| List.reverse board
    in
        winnerHorizontal [ diagonal1, diagonal2 ]


hasEmptySqaures : List (List Int) -> Bool
hasEmptySqaures board =
    List.any (\a -> List.any (\n -> n == 0) a) board


gameResult : Board -> Int
gameResult board =
    let
        boardWithLists =
            toList <| map toList board

        wd =
            winnerDiagonal boardWithLists

        wh =
            winnerHorizontal boardWithLists

        wv =
            winnerVertical boardWithLists

        hes =
            hasEmptySqaures boardWithLists
    in
        if wd || wh || wv then
            2
        else if hes then
            0
        else
            1



-- Update


type Msg
    = MakeMove Int Int
    | NewGame


update : Msg -> GameState -> GameState
update msg model =
    case msg of
        MakeMove r c ->
            makeMove model r c

        NewGame ->
            getInitialModel



-- View


renderTile : Int -> Int -> Int -> Html Msg
renderTile r c tile =
    let
        ddiivv =
            div [ class "tile", onClick <| MakeMove r c ]
    in
        case tile of
            1 ->
                ddiivv [ text "X" ]

            2 ->
                ddiivv [ text "O" ]

            _ ->
                ddiivv [ text " " ]


renderRow : Int -> Array Int -> List (Html Msg)
renderRow r row =
    toList <| indexedMap (renderTile r) row


flattenList : List (List a) -> List a
flattenList matrix =
    List.foldl List.append [] matrix


renderBoard : Board -> List (Html Msg)
renderBoard board =
    flattenList <| toList <| indexedMap renderRow board


renderResult : GameState -> Html Msg
renderResult state =
    let
        currentPlayer player1 =
            if player1 then
                "O"
            else
                "X"

        result =
            gameResult state.board

        header =
            h2 [ class "h2" ]
    in
        case result of
            1 ->
                header [ text <| "Game is a tie !!" ]

            2 ->
                header [ text <| "Game over " ++ (currentPlayer <| not state.player1) ++ " won" ]

            _ ->
                header [ text <| "Current Player: " ++ currentPlayer state.player1 ]


renderGame : GameState -> List (Html Msg)
renderGame state =
    (renderResult state) :: (renderBoard state.board)


view : GameState -> Html Msg
view model =
    div [ class "container" ]
        [ button [ id "new-game", onClick NewGame ]
            [ text "New Game" ]
        , div [ id "game" ] <| renderGame model
        ]
