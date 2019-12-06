module Main exposing (..)

import Browser
import Dict exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type GamePhase
    = Placing Placement
    | Playing GamePlay
    | GameOver (List AxialCoord) Board


type alias Model =
    { phase : GamePhase
    }


init : Model
init =
    { phase = Placing newPlacement
    }


type alias AxialCoord =
    ( Int, Int )


type Player
    = WhitePlayer
    | BlackPlayer


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        WhitePlayer ->
            BlackPlayer

        BlackPlayer ->
            WhitePlayer


type Piece
    = DvonnPiece
    | WhitePiece
    | BlackPiece


type alias Stack =
    -- A non empty list of pieces
    ( Piece, List Piece )


type alias Board =
    -- Location of dvonn pieces
    { dvonn : Set AxialCoord

    -- Location -> Stacks at that location
    , stacks : Dict AxialCoord Stack
    }


type alias Placement =
    -- Placement phase
    { dvonn : Int
    , black : Int
    , white : Int
    , player : Player
    , board : Board
    , hexes : List AxialCoord
    }


newPlacement : Placement
newPlacement =
    -- Placement for classic game of Dvonn
    Placement 3 23 23 WhitePlayer emptyBoard (hexesGenerator 8 2)


type alias GamePlay =
    -- Playing phase
    { player : Player
    , selected : Maybe AxialCoord
    , board : Board
    , hexes : List AxialCoord
    }


gamePlayFromPlacement : Placement -> GamePlay
gamePlayFromPlacement { player, board, hexes } =
    { player = switchPlayer player, board = board, hexes = hexes, selected = Nothing }



-- UPDATE


type Msg
    = NoOp
    | ClickedHex AxialCoord
    | RandoPlacement
    | Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ClickedHex coord ->
            case model.phase of
                Placing placement ->
                    placementClick coord placement

                Playing gameplay ->
                    playingClick coord gameplay

                GameOver _ _ ->
                    model

        RandoPlacement ->
            case model.phase of
                Placing placement ->
                    randomPlacement placement

                Playing _ ->
                    model

                GameOver _ _ ->
                    model

        Restart ->
            init



-- Placing phase


placementClick : AxialCoord -> Placement -> Model
placementClick coord placement =
    let
        updated =
            doPlacement coord placement

        placementOver =
            updated.dvonn
                == 0
                && updated.white
                == 0
                && updated.black
                == 0
    in
    if placementOver then
        { phase = Playing (gamePlayFromPlacement updated) }

    else
        { phase = Placing updated }


nextPlacement : Placement -> Player
nextPlacement { player, white, black, dvonn } =
    if dvonn > 0 then
        switchPlayer player

    else if white == 0 then
        BlackPlayer

    else if black == 0 then
        WhitePlayer

    else
        switchPlayer player


doPlacement : AxialCoord -> Placement -> Placement
doPlacement coord placement =
    if hasStack placement.board coord then
        placement

    else if placement.dvonn > 0 then
        let
            updatePiece =
                { placement | dvonn = placement.dvonn - 1 }

            updatePlayer =
                { updatePiece | player = nextPlacement updatePiece }
        in
        { updatePlayer
            | board = placePiece placement.board coord DvonnPiece
        }

    else
        let
            ( updatePiece, piece ) =
                case placement.player of
                    WhitePlayer ->
                        ( { placement | white = placement.white - 1 }, WhitePiece )

                    BlackPlayer ->
                        ( { placement | black = placement.black - 1 }, BlackPiece )

            updatePlayer =
                { updatePiece | player = nextPlacement updatePiece }
        in
        { updatePlayer
            | board = placePiece placement.board coord piece
        }


randomPlacement : Placement -> Model
randomPlacement placement =
    let
        placed =
            Set.fromList (Dict.keys placement.board.stacks)

        unplaced =
            Set.diff (Set.fromList placement.hexes) placed

        updated =
            Set.foldl doPlacement placement unplaced
    in
    { phase = Playing (gamePlayFromPlacement updated) }



-- Playing phase


playingClick : AxialCoord -> GamePlay -> Model
playingClick coord playing =
    let
        maybeStack =
            getStack playing.board coord

        { player, board } =
            playing
    in
    case maybeStack of
        Nothing ->
            { phase = Playing { playing | selected = Nothing } }

        Just stack ->
            case playing.selected of
                -- Nothing selected
                Nothing ->
                    if
                        List.isEmpty (movesFrom board coord)
                            || not (isStackOwnedBy player stack)
                    then
                        -- No valid moves or stack is owned by someone else, do nothing.
                        { phase = Playing playing }

                    else
                        -- Valid moves, stack is owned by current players, coord is selected
                        { phase = Playing { playing | selected = Just coord } }

                -- Something selected
                Just selected ->
                    if List.member coord (movesFrom board selected) then
                        -- Is a valid move, do it and change turns/end game
                        let
                            newBoard =
                                doMove board selected coord

                            maybeNextPlayer =
                                nextPlaying newBoard player
                        in
                        case maybeNextPlayer of
                            Nothing ->
                                { phase = GameOver playing.hexes newBoard }

                            Just next ->
                                { phase =
                                    Playing
                                        { playing
                                            | board = newBoard
                                            , player = next
                                            , selected = Nothing
                                        }
                                }

                    else
                        -- Clicked is not a valid move, unselect
                        { phase = Playing { playing | selected = Nothing } }


nextPlaying : Board -> Player -> Maybe Player
nextPlaying board player =
    let
        whiteMoves =
            not (List.isEmpty (movesFor board WhitePlayer))

        blackMoves =
            not (List.isEmpty (movesFor board BlackPlayer))
    in
    if whiteMoves && blackMoves then
        Just (switchPlayer player)

    else if whiteMoves then
        Just WhitePlayer

    else if blackMoves then
        Just BlackPlayer

    else
        Nothing


prunePieces : Board -> Board
prunePieces board =
    let
        currentStacks =
            Dict.keys board.stacks
                |> Set.fromList

        keepStacks =
            floodfill (neighbours board 1) board.dvonn board.dvonn
    in
    { board
        | stacks =
            Set.diff currentStacks keepStacks
                |> Set.foldl Dict.remove board.stacks
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.phase of
        Placing placement ->
            viewPlacement placement

        Playing gameplay ->
            viewPlaying gameplay

        GameOver hexes board ->
            viewGameOver hexes board


bgAttribs : List (Svg.Attribute Msg)
bgAttribs =
    [ fill "blanchedalmond", stroke "burlywood" ]


viewBoard : List (Svg Msg) -> Svg Msg
viewBoard c =
    svg
        [ width "400"
        , height "400"
        , viewBox "-200 -200 800 600"
        ]
        c


viewPlacement : Placement -> Html Msg
viewPlacement placement =
    div []
        [ viewBoard
            [ viewHexes 30 bgAttribs placement.hexes
            , viewStacks 30 placement.board
            ]
        , Html.text
            (case placement.player of
                WhitePlayer ->
                    "Whites turn"

                BlackPlayer ->
                    "Blacks turn"
            )
        , button
            [ onClick RandoPlacement ]
            [ Html.text "Auto Placement" ]
        ]


viewPlaying : GamePlay -> Html Msg
viewPlaying playing =
    let
        highlights =
            case playing.selected of
                Nothing ->
                    [ viewHexes 30
                        [ fill "sandybrown" ]
                        (movesFor playing.board playing.player)
                    ]

                Just coord ->
                    [ viewHexes 30 [ fill "sandybrown" ] [ coord ]
                    , viewHexes 30
                        [ fill "lightgreen" ]
                        (movesFrom playing.board coord)
                    ]
    in
    div []
        [ viewBoard
            (viewHexes 30 bgAttribs playing.hexes
                :: highlights
                ++ [ viewStacks 30 playing.board ]
            )
        , Html.text
            (case playing.player of
                WhitePlayer ->
                    "Whites turn"

                BlackPlayer ->
                    "Blacks turn"
            )
        ]


winner : Board -> Maybe Player
winner board =
    let
        whiteStackSize =
            stacksFor board WhitePlayer
                |> Dict.values
                |> List.map stackSize
                |> List.foldl (+) 0

        blackStackSize =
            stacksFor board BlackPlayer
                |> Dict.values
                |> List.map stackSize
                |> List.foldl (+) 0
    in
    if whiteStackSize > blackStackSize then
        Just WhitePlayer

    else if blackStackSize > whiteStackSize then
        Just BlackPlayer

    else
        Nothing


viewGameOver : List AxialCoord -> Board -> Html Msg
viewGameOver hexes board =
    div []
        [ viewBoard
            [ viewHexes 30 bgAttribs hexes
            , viewStacks 30 board
            ]
        , Html.text
            (case winner board of
                Nothing ->
                    "You both lose"

                Just player ->
                    case player of
                        WhitePlayer ->
                            "Whites Wins!"

                        BlackPlayer ->
                            "Blacks wins!"
            )
        , button
            [ onClick Restart ]
            [ Html.text "Start again" ]
        ]


viewHexes : Float -> List (Svg.Attribute Msg) -> List AxialCoord -> Svg Msg
viewHexes size attribs coords =
    g
        []
        (List.map (viewHexAt attribs size) coords)


pieceToColor : Piece -> String
pieceToColor piece =
    case piece of
        WhitePiece ->
            "floralwhite"

        DvonnPiece ->
            "deeppink"

        BlackPiece ->
            "grey"


strokeColor : Piece -> String
strokeColor piece =
    case piece of
        WhitePiece ->
            "black"

        DvonnPiece ->
            "deeppink"

        BlackPiece ->
            "black"


viewStack : Float -> AxialCoord -> Stack -> Svg Msg
viewStack hexSize coord ( piece, rest ) =
    let
        ( x, y ) =
            axialToPixel hexSize coord

        size =
            stackSize ( piece, rest )

        stack =
            List.reverse (piece :: rest)

        deltas =
            List.range 0 size

        z =
            List.map2 (\a b -> ( a, toFloat b )) stack deltas

        radius =
            0.7 * hexSize

        radiusDelta =
            radius / toFloat size

        circles =
            List.map
                (\( c, d ) ->
                    circle
                        [ cx (String.fromFloat x)
                        , cy (String.fromFloat y)
                        , r (String.fromFloat (radius - (radiusDelta * d)))
                        , fill (pieceToColor c)
                        , stroke (strokeColor c)
                        , onClick (ClickedHex coord)
                        ]
                        []
                )
                z
    in
    g
        []
        circles


viewStacks : Float -> Board -> Svg Msg
viewStacks size board =
    g
        []
        (Dict.toList board.stacks
            |> List.map (\( a, b ) -> viewStack size a b)
        )


viewHexAt : List (Svg.Attribute Msg) -> Float -> AxialCoord -> Svg Msg
viewHexAt attribs hexSize coords =
    let
        ( x, y ) =
            axialToPixel hexSize coords

        p =
            List.map toFloat (List.range 0 5)
                |> List.map (\a -> a * pi / 3)
                |> List.map (\a -> ( x + hexSize * sin a, y + hexSize * cos a ))

        pointsString =
            List.map (\( i, j ) -> String.fromFloat i ++ "," ++ String.fromFloat j) p
                |> List.intersperse " "
                |> List.foldl (++) ""
    in
    polygon
        (onClick (ClickedHex coords) :: points pointsString :: attribs)
        []



-- UTILS


floodfill : (comparable -> List comparable) -> Set comparable -> Set comparable -> Set comparable
floodfill neighbourFn colored recentlyColored =
    let
        newlyColored =
            Set.diff
                (Set.toList recentlyColored
                    |> List.map neighbourFn
                    |> List.concat
                    |> Set.fromList
                )
                colored

        newColored =
            Set.union colored newlyColored
    in
    if Set.isEmpty newlyColored then
        colored

    else
        Set.union newColored (floodfill neighbourFn newColored newlyColored)


maybeHasValue : Maybe a -> Bool
maybeHasValue a =
    case a of
        Nothing ->
            False

        _ ->
            True


foldlish : (a -> b -> b) -> b -> List a -> List b
foldlish fn start list =
    case list of
        [] ->
            []

        a :: xs ->
            let
                applied =
                    fn a start
            in
            applied :: foldlish fn applied xs


zip : a -> b -> ( a, b )
zip a b =
    ( a, b )



-- Axial Coordinate helpers


addAxial : AxialCoord -> AxialCoord -> AxialCoord
addAxial ( q1, r1 ) ( q2, r2 ) =
    ( q1 + q2, r1 + r2 )


multAxial : AxialCoord -> Int -> AxialCoord
multAxial ( q, r ) v =
    ( v * q, v * r )


axialMult : Int -> AxialCoord -> AxialCoord
axialMult v ( q, r ) =
    ( v * q, v * r )


axialDirections : List AxialCoord
axialDirections =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]


axialToPixel : Float -> AxialCoord -> ( Float, Float )
axialToPixel size ( q, r ) =
    ( size * (sqrt 3 * toFloat q + toFloat r * sqrt 3 / 2)
    , size * (3 / 2) * toFloat r
    )


boardOutline : Int -> Int -> List Int
boardOutline width diag =
    [ width, diag, diag, width, diag, diag ]



-- Stack helpers


newStack : Piece -> Stack
newStack piece =
    ( piece, [] )


stackSize : Stack -> Int
stackSize ( _, l ) =
    1 + List.length l


addStack : Stack -> Stack -> Stack
addStack ( p1, l1 ) ( p2, l2 ) =
    ( p1, l1 ++ (p2 :: l2) )


isStackOwnedBy : Player -> Stack -> Bool
isStackOwnedBy player ( piece, _ ) =
    case ( piece, player ) of
        ( WhitePiece, WhitePlayer ) ->
            True

        ( BlackPiece, BlackPlayer ) ->
            True

        _ ->
            False



-- Board helpers


emptyBoard : Board
emptyBoard =
    Board Set.empty Dict.empty


placePiece : Board -> AxialCoord -> Piece -> Board
placePiece board coord piece =
    { board
        | stacks = Dict.insert coord (newStack piece) board.stacks
        , dvonn =
            case piece of
                DvonnPiece ->
                    Set.insert coord board.dvonn

                _ ->
                    board.dvonn
    }


doMove : Board -> AxialCoord -> AxialCoord -> Board
doMove board from to =
    let
        fromStack =
            getStack board from

        toStack =
            getStack board to

        isFromDvonn =
            Set.member from board.dvonn

        updatedStack =
            Maybe.map2 addStack fromStack toStack
    in
    case updatedStack of
        Just stack ->
            { board
                | stacks =
                    Dict.insert to stack board.stacks
                        |> Dict.remove from
                , dvonn =
                    if isFromDvonn then
                        Set.remove from board.dvonn
                            |> Set.insert to

                    else
                        board.dvonn
            }
                |> prunePieces

        Nothing ->
            board


neighbours : Board -> Int -> AxialCoord -> List AxialCoord
neighbours board distance coord =
    List.map (axialMult distance) axialDirections
        |> List.map (addAxial coord)
        |> List.filter (hasStack board)


hasStack : Board -> AxialCoord -> Bool
hasStack board coord =
    Dict.member coord board.stacks


getStack : Board -> AxialCoord -> Maybe Stack
getStack board coord =
    Dict.get coord board.stacks


stacksFor : Board -> Player -> Dict AxialCoord Stack
stacksFor board player =
    Dict.filter (\_ -> isStackOwnedBy player) board.stacks


movesFor : Board -> Player -> List AxialCoord
movesFor board player =
    stacksFor board player
        |> Dict.keys
        |> List.filter (\c -> not (List.isEmpty (movesFrom board c)))


movesFrom : Board -> AxialCoord -> List AxialCoord
movesFrom board coord =
    let
        size =
            getStack board coord
                |> Maybe.map stackSize
                |> Maybe.withDefault 0

        surrounded =
            List.length (neighbours board 1 coord) == 6
    in
    if size > 0 && not surrounded then
        neighbours board size coord

    else
        []



-- Board generation


hexesInDirection : ( Int, AxialCoord ) -> AxialCoord -> List AxialCoord
hexesInDirection ( amount, dir ) origin =
    List.range 0 (amount - 1)
        |> List.map (multAxial dir)
        |> List.map (addAxial origin)


genRing : Int -> Int -> Int -> List AxialCoord
genRing width diag rad =
    let
        -- List of (amount, directions) from corner to corner
        origin =
            multAxial ( 0, 1 ) rad

        directions =
            List.map2
                zip
                (boardOutline (width - rad) (diag - rad))
                axialDirections

        -- Positions of the corners
        origins =
            origin
                :: foldlish
                    addAxial
                    origin
                    (List.map (\( a, d ) -> multAxial d a) directions)
    in
    if List.any (\( a, _ ) -> a == 0) directions then
        -- Cover case where it's a single line and not full ring
        hexesInDirection ( width - rad + 1, ( 1, 0 ) ) origin

    else
        -- Add points from each origin in given direction
        List.map2 hexesInDirection directions origins
            |> List.concat


hexesGenerator : Int -> Int -> List AxialCoord
hexesGenerator width diag =
    List.range 0 (Basics.min width diag)
        |> List.map (genRing width diag)
        |> List.concat
