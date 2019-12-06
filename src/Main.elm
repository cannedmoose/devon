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

stackOwnedBy : Player -> Stack -> Bool
stackOwnedBy player (piece, _) =
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

isDvonn : Board -> AxialCoord -> Bool
isDvonn board coord =
  Set.member coord board.dvonn

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
  | GameOver (List AxialCoord) Board 

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
  | Restart

update msg model =
  case msg of
    NoOp -> model
    ClickedHex coord -> (
      case model.phase of
        Placing placement -> (placementClick coord placement)
        Playing gameplay -> (playingClick coord gameplay)
        GameOver _ _ -> model 
      )
    RandoPlacement -> (
      case model.phase of
        Placing placement -> (randomPlacement placement)
        Playing gameplay -> model
        GameOver _ _ -> model
      )
    Restart -> (init)

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

stacksFor : Board -> Player -> Dict AxialCoord Stack
stacksFor board player =
  Dict.filter (\c -> stackOwnedBy player) board.stacks

movesFor : Board -> Player -> Set AxialCoord
movesFor board player =
  stacksFor board player
    |> Dict.keys 
    |> List.filter (\c -> not (List.isEmpty (validMoves board c)) )
    |> Set.fromList

nextPlaying : Board -> Player -> Maybe Player
nextPlaying board player =
  let
    whiteMoves = not (Set.isEmpty (movesFor board WhitePlayer))
    blackMoves = not (Set.isEmpty (movesFor board BlackPlayer))
  in
    if whiteMoves && blackMoves then
      Just (switchPlayer player)
    else if whiteMoves then
      Just WhitePlayer
    else if blackMoves then
      Just BlackPlayer
    else
      Nothing

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

neighbours : Board -> AxialCoord -> List AxialCoord
neighbours board coord =
  axialDirections
    |> List.map (addAxial coord)
    |> List.filter (hasStack board) 

-- Recursively flood fill a board
-- Colored are all colored peices, recent are peices colored in last call
-- For first call these should be the same.
floodfill : Board -> Set AxialCoord  -> Set AxialCoord -> Set AxialCoord
floodfill board colored recent =
  let
    recentNeighbours = Set.map (neighbours board) recent
      |> Set.toList |> List.concat |> Set.fromList
      |> (\s -> Set.diff s colored)
    newColored = Set.union colored recentNeighbours
  in
    if Set.isEmpty recentNeighbours then
      colored
    else
      Set.union newColored (floodfill board newColored recentNeighbours)

prunePieces : Board -> Board
prunePieces board =
  let
    currentStacks = Dict.keys board.stacks
      |> Set.fromList
    keepStacks = floodfill board board.dvonn board.dvonn
  in
    { board
    | stacks = Set.diff currentStacks keepStacks
      |> Set.foldl Dict.remove board.stacks
    }


doMove : Board -> AxialCoord -> AxialCoord -> Board
doMove board from to =
  let
    fromStack = getStack board from
    toStack = getStack board to
    isFromDvonn = isDvonn board from
    updatedStack = Maybe.map2 addStack fromStack toStack
  in
    case updatedStack of
      Just stack -> (
        { board
        | stacks = Dict.insert to stack board.stacks
          |> Dict.remove from
        , dvonn = if isFromDvonn then 
            Set.remove from board.dvonn
            |> Set.insert to
          else board.dvonn 
        } |> prunePieces
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
          || not (stackOwnedBy player stack) then
        -- No valid moves or stack is owned by someone else, do nothing.
          { phase = Playing playing }
        else
          -- Valid moves, stack is owned by current players, coord is selected
          { phase = Playing { playing | selected = Just coord } }
      -- Something selected
      Just selected ->
        if List.member coord (validMoves board selected) then
          -- Is a valid move, do it and change turns/end game
          let
            newBoard = (doMove board selected coord)
            maybeNextPlayer = nextPlaying newBoard player
          in
            case maybeNextPlayer of
              Nothing -> { phase = (GameOver playing.hexes newBoard) }
              Just next -> (
                { phase = Playing { playing 
                  | board = newBoard
                  , player = next
                  , selected = Nothing
                  } 
                }
                )
        else
          -- Clicked is not a valid move, unselect
          { phase = Playing { playing | selected = Nothing } }
        )

-- VIEW
view model =
  case model.phase of
    Placing placement -> (viewPlacement placement)
    Playing gameplay -> (viewPlaying gameplay)
    GameOver hexes board -> (viewGameOver hexes board)

bgAttribs = [fill "blanchedalmond", stroke "burlywood"]

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
  let
    highlights = case playing.selected of
      Nothing -> (
        [viewHexes 30 [fill "sandybrown"]
          (Set.toList (movesFor playing.board playing.player))]
        )
      Just coord -> (
        [ viewHexes 30 [fill "sandybrown"] [coord]
        , viewHexes 30 [fill "lightgreen"]
          (validMoves playing.board coord)]
        )
      
  in
    div []
      [ viewBoard
        ([ viewHexes 30 bgAttribs playing.hexes
        ] ++ highlights ++
        [ viewStacks 30 playing.board
        ])
      , Html.text 
        (case playing.player of
          WhitePlayer -> ("Whites turn")
          BlackPlayer -> ("Blacks turn")
        )]

winner : Board -> Maybe Player
winner board =
  let
    whiteStackSize = stacksFor board WhitePlayer
      |> Dict.values
      |> List.map stackSize
      |> List.foldl (+) 0 
    blackStackSize = stacksFor board BlackPlayer
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
      ( case winner board of
        Nothing -> ("You both lose")
        Just player -> (
            case player of
              WhitePlayer -> ("Whites Wins!")
              BlackPlayer -> ("Blacks wins!")
          )
      )
    , button
      [onClick Restart]
      [Html.text "Start again"]
    ]

viewHexes : Float -> List (Svg.Attribute Msg) -> List AxialCoord -> Svg Msg
viewHexes size attribs coords =
  g
      []
      (List.map (viewHexAt attribs size) coords)

pieceToColor : Piece -> String
pieceToColor piece =
  case piece of
    WhitePiece -> ("floralwhite")
    DvonnPiece -> ("deeppink")
    BlackPiece -> ("grey")

strokeColor piece =
  case piece of
    WhitePiece -> ("black")
    DvonnPiece -> ("deeppink")
    BlackPiece -> ("black")

viewStack : Float -> AxialCoord -> Stack -> Svg Msg
viewStack hexSize coord (piece, rest) =
  let
    (x, y) = axialToPixel hexSize coord
    size = stackSize (piece, rest)
    sizeDiv2 = size // 2
    sizeMod2 = modBy 2 size
    stack = List.reverse (piece :: rest)
    deltas = List.range 0 size
    z = List.map2 (\a b -> (a, toFloat b)) stack deltas
    radius = 0.7*hexSize
    radiusDelta = radius/(toFloat size)
    circles = List.map (\(c, d) ->
      circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r (String.fromFloat (radius - (radiusDelta * d)) )
        , fill (pieceToColor c)
        , stroke (strokeColor c)
        , onClick (ClickedHex coord)
        ]
        []
      ) z
  in
    g
      []
      circles
  

viewStacks : Float -> Board -> Svg Msg
viewStacks size board =
  g
    []
    (Dict.toList board.stacks
      |> List.map (\(a, b) -> viewStack size a b))

viewHexAt :  List (Svg.Attribute Msg) -> Float  -> AxialCoord -> Svg Msg
viewHexAt attribs hexSize coords =
  let
    (x, y) = axialToPixel hexSize coords
    p = List.map toFloat (List.range 0 5) 
      |> List.map (\a -> a * pi / 3 )
      |> List.map (\a -> (x + hexSize * (sin a), y + hexSize * (cos a)))
    pointsString = List.map (\(i,j) -> String.fromFloat i ++ "," ++ String.fromFloat j) p
      |> List.intersperse " "
      |> List.foldl (++) ""
  in
    polygon
          (onClick (ClickedHex coords) :: points pointsString :: attribs)
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