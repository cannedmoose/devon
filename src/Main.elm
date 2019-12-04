module Main exposing(..)
import Browser
import Dict exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias AxialCoord = (Int, Int)

type Player
  = WhitePlayer
  | BlackPlayer

type Piece
  = DvonnPiece
  | WhitePiece
  | BlackPiece

-- Stack is a none empty ist of pieces
type alias Stack = (Piece, List Piece)
newStack: Piece -> Stack
newStack piece = (piece, [])

stackOwnedBy : Stack -> Player -> Bool
stackOwnedBy (piece, _) player =
  case (piece, player) of
    (WhitePiece, WhitePlayer) -> True
    (BlackPiece, BlackPlayer) -> True
    _ -> False

addStack : Stack -> Stack -> Stack
addStack (p1, l1) (p2, l2)
  = (p1, l1 ++ (p2 :: l2))

type alias Board =
  -- Location of dvonn pieces
  { dvonn : Set AxialCoord 
  -- Location -> Stacks at that location
  , stacks : Dict AxialCoord Stack
  }

emptyBoard : Board
emptyBoard = Board Set.empty Dict.empty

hasStack : Board -> AxialCoord -> Bool
hasStack board coord =
  Dict.member coord board.stacks

getStack : Board -> AxialCoord -> Maybe Stack
getStack board coord =
  Dict.get coord board.stacks

addPiece : Board -> AxialCoord -> Piece -> Board
addPiece board coord piece =
  { board
  | stacks = Dict.insert coord (newStack piece) board.stacks
  , dvonn = case piece of
      DvonnPiece -> (Set.insert coord board.dvonn)
      _ -> (board.dvonn)
  }

type alias Placement =
  { dvonn : Int
  , black : Int
  , white : Int
  , player : Player
  , board : Board
  , hexes : List AxialCoord
  }

type alias GamePlay =
  { player : Player
  , selected : Maybe AxialCoord
  , board : Board
  , hexes : List AxialCoord
  }

gamePlayFromPlacement : Placement -> GamePlay
gamePlayFromPlacement { player, board, hexes } =
  {player = switchPlayer player, board = board, hexes = hexes, selected = Nothing}


-- Placement for classic game of Dvonn
newPlacement : Placement
newPlacement = Placement 3 23 23 WhitePlayer emptyBoard (hexesGenerator 8 2)

type GamePhase 
  = Placing Placement
  | Playing GamePlay

type alias Model =
  { phase : GamePhase
  }

init : Model
init =
  { phase = Placing newPlacement
  }

-- UPDATE
type Msg
  = NoOp
  | ClickedHex AxialCoord
  | RandoPlacement

update msg model =
  case msg of
    NoOp -> model
    ClickedHex coord -> (
      case model.phase of
        Placing placement -> (placementClick coord placement)
        Playing gameplay -> (playingClick coord gameplay)
      )
    RandoPlacement -> (
      case model.phase of
        Placing placement -> (randomPlacement placement)
        Playing gameplay -> model
      )

switchPlayer : Player -> Player
switchPlayer player =
  case player of
    WhitePlayer -> BlackPlayer
    BlackPlayer -> WhitePlayer 

nextPlacement : Placement -> Player
nextPlacement {player, white, black, dvonn} =
  if dvonn > 0 then switchPlayer player
  else if white == 0 then BlackPlayer
  else if black == 0 then WhitePlayer
  else switchPlayer player

doPlacement: AxialCoord -> Placement -> Placement
doPlacement coord placement = 
  if hasStack placement.board coord then placement
  else if placement.dvonn > 0 then
    let
      updatePiece = { placement | dvonn = placement.dvonn - 1 }
      updatePlayer = { updatePiece | player = nextPlacement updatePiece }
      updateBoard = { updatePlayer
         | board = addPiece placement.board coord DvonnPiece }
    in updateBoard
  else
    let
      (updatePiece, piece) = case placement.player of
        WhitePlayer -> ({ placement | white = placement.white - 1 }, WhitePiece)
        BlackPlayer -> ({ placement | black = placement.black - 1 }, BlackPiece)
      updatePlayer = {updatePiece | player = nextPlacement updatePiece}
      updateBoard = { updatePlayer
         | board = addPiece placement.board coord piece }
    in updateBoard

placementClick : AxialCoord -> Placement -> Model
placementClick coord placement =
  let
    updated = doPlacement coord placement
    placementOver =
        updated.dvonn == 0 && 
        updated.white == 0 &&
        updated.black == 0 
  in
    if placementOver then
      {phase = Playing (gamePlayFromPlacement updated)}
    else 
      {phase = Placing updated}

randomPlacement : Placement -> Model
randomPlacement placement =
  let
    placed = Set.fromList (Dict.keys placement.board.stacks)
    unplaced = Set.diff (Set.fromList placement.hexes) placed
    updated = Set.foldl doPlacement placement unplaced
  in
    {phase = Playing (gamePlayFromPlacement updated)}

maybeHasValue : Maybe a -> Bool
maybeHasValue a =
  case a of
    Nothing -> False
    _ -> True  

isSurrounded : Board -> AxialCoord -> Bool
isSurrounded board coord =
  List.map (addAxial coord) axialDirections
    |> List.all (hasStack board)

stackSize : Stack -> Int
stackSize (a, l) = 1 + List.length l 

validMoves : Board -> AxialCoord -> List AxialCoord
validMoves board coord =
  let
    size = getStack board coord
      |> Maybe.map stackSize
      |> Maybe.withDefault 0
  in
    if size > 0 && not (isSurrounded board coord) then
      List.map (axialMult size) axialDirections
        |> List.map (addAxial coord)
        |> List.filter (hasStack board) 
    else
      []

doMove : Board -> AxialCoord -> AxialCoord -> Board
doMove board from to =
  let
    fromStack = getStack board from
    toStack = getStack board to
    updatedStack = Maybe.map2 addStack fromStack toStack
  in
    case updatedStack of
      Just stack -> (
        { board
        | stacks = Dict.insert to stack board.stacks
          |> Dict.remove from
        }
        )
      Nothing -> board

playingClick : AxialCoord -> GamePlay -> Model
playingClick coord playing
  =
  let
    maybeStack = getStack playing.board coord
    {player, board} = playing
  in
    case maybeStack of
    Nothing -> { phase = Playing { playing | selected = Nothing } }
    Just stack -> (
      case playing.selected of
      -- Nothing selected
      Nothing ->
        if (validMoves board coord |> List.isEmpty)
          || not (stackOwnedBy stack player) then
        -- No valid moves or stack is owned by someone else, do nothing.
          { phase = Playing playing }
        else
          -- Valid moves, stack is owned by current players, coord is selected
          { phase = Playing { playing | selected = Just coord } }
      -- Something selected
      Just selected ->
        if List.member coord (validMoves board selected) then
          -- Is a valid move, do it and change turns
          { phase = Playing { playing 
            | board = (doMove board selected coord)
            , player = switchPlayer player
            , selected = Nothing
            } }
        else
          -- Clicked is not a valid move, unselect
          { phase = Playing { playing | selected = Nothing } }
        )

-- VIEW
view model =
  case model.phase of
    Placing placement -> (viewPlacement placement)
    Playing gameplay -> (viewPlaying gameplay)

viewBoard : List AxialCoord -> Board -> Maybe AxialCoord -> Player -> Svg Msg
viewBoard hexes board selected player =
  svg
      [ width "400"
      , height "400"
      , viewBox "-200 -200 800 600"
      ]
      (hexes |> List.map (viewBoardHex board selected player))

viewPlacement : Placement -> Html Msg
viewPlacement placement =
  div []
    [ viewBoard placement.hexes placement.board Nothing placement.player
    , Html.text 
      ( case placement.player of
        WhitePlayer -> ("Whites turn")
        BlackPlayer -> ("Blacks turn")
      )
    , button
      [onClick RandoPlacement]
      [Html.text "Auto Placement"]
    ]
  
viewPlaying : GamePlay -> Html Msg
viewPlaying playing =
  div []
    [ viewBoard playing.hexes playing.board playing.selected playing.player
    , Html.text 
      ( case playing.player of
        WhitePlayer -> ("Whites turn")
        BlackPlayer -> ("Blacks turn")
      )]

-- TODO should have some kinf of hex context
-- Want board, selected, current player
viewBoardHex : Board -> Maybe AxialCoord -> Player -> AxialCoord -> Svg Msg
viewBoardHex board maybeSelected player coord =
  let
    maybePiece = getStack board coord
    (color, isPlayers) = case maybePiece of
      Nothing -> ("grey", False)
      Just (piece , xs) -> (case piece of
        WhitePiece -> ("white", stackOwnedBy (piece , xs) player)
        DvonnPiece -> ("red", False)
        BlackPiece -> ("black", stackOwnedBy (piece , xs) player))
    selectedColor = case maybeSelected of
      Nothing -> (
        if List.isEmpty (validMoves board coord) || not isPlayers then Nothing
        else Just "orange"
        )
      Just selected -> (
        if selected == coord then Just "green"
        else if List.member coord (validMoves board selected) then Just "yellow"
        else Nothing 
        )
  in
    g
      []
      ([ viewCircleAt 30 20 color coord ]
        ++  (
        Maybe.map (viewCircleAt 30 5) selectedColor
        |> Maybe.map (\a-> [a coord]) 
        |> Maybe.withDefault []
        ))

viewCircleAt : Float -> Float -> String -> AxialCoord -> Svg Msg
viewCircleAt hexSize radius c coords =
  let
    (x, y) = axialToPixel hexSize coords
  in
    circle
          [ cx (String.fromFloat x)
          , cy (String.fromFloat y)
          , r (String.fromFloat radius)
          , fill c
          , stroke "red"
          , onClick (ClickedHex coords)
          ]
          []


-- UTILS
foldlish : (a -> b -> b) -> b -> List a -> List b
foldlish fn start list =
  case list of
    [] -> []
    a :: xs -> (
      let
        applied = fn a start 
      in 
        applied :: foldlish fn applied xs
      )
zip : a -> b -> (a, b)
zip a b = (a, b)

-- Axial
addAxial : AxialCoord -> AxialCoord -> AxialCoord
addAxial (q1, r1) (q2, r2) = (q1 + q2, r1 + r2)

multAxial : AxialCoord -> Int -> AxialCoord 
multAxial (q, r) v = (v * q, v * r)

axialMult : Int -> AxialCoord  -> AxialCoord 
axialMult v (q, r) = (v * q, v * r)

axialDirections : List AxialCoord
axialDirections =
  [ (1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)]

boardRange : Int -> Int -> List Int
boardRange width diag =
  [ width, diag, diag, width, diag, diag]

axialToPixel : Float -> AxialCoord -> (Float, Float)
axialToPixel size (q, r) =
  ( (size * ((sqrt 3) * (toFloat q) + (toFloat r) * (sqrt 3) / 2))
  , (size * ( 3 / 2 ) * (toFloat r))
  )

-- Board generation
hexesInDirection :  (Int, AxialCoord) -> AxialCoord -> List AxialCoord
hexesInDirection (amount, dir) origin =
  List.range 0 (amount - 1)
    |> List.map (multAxial dir)
    |> List.map (addAxial origin) 

genRing : Int -> Int -> Int -> List AxialCoord
genRing width diag rad =
  let
    -- List of (amount, directions) from corner to corner
    origin = (multAxial (0, 1) rad)
    directions = List.map2
      (zip)
      (boardRange (width - rad) (diag - rad))
      axialDirections
    -- Positions of the corners
    origins = origin :: (foldlish
      addAxial
      origin
      (List.map (\(a, d) -> multAxial d a) directions))
  in
    if List.any (\(a, d) -> a == 0) directions then
      -- Cover case where it's a single line and not full ring
      hexesInDirection (width - rad + 1, (1, 0)) origin
    else
      -- Add points from each origin in given direction
      List.map2 hexesInDirection directions origins
      |> List.concat

hexesGenerator : Int -> Int -> List AxialCoord
hexesGenerator width diag =
  List.range 0 (Basics.min width diag)
    |> List.map (genRing width diag)
    |> List.concat