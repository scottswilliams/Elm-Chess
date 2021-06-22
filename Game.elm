module Game exposing (..)
import Array exposing (..)
import Char exposing (..)

type Color = White | Black
type Piece = Pawn | Knight | Bishop | Rook | Queen | King

centerWeight : Float
centerWeight = 0.5

searchDepth = 2

type alias Chessman =
 { piece : Piece,
   color : Color }

type Square = Empty | C Chessman

--Remembers where black/white's pieces are so that detecting check/castling does not require searching the whole board for a side's pieces

type alias Board = (Array Square)
type alias State = 
 {board : Board, 
  whitePieces : List Int, 
  blackPieces : List Int,
  whiteCanCastleShort : Bool,
  blackCanCastleShort : Bool,
  whiteCanCastleLong : Bool,
  blackCanCastleLong : Bool,
  whiteKingSquare : Int,
  blackKingSquare : Int,
  enPassantSquare : Int,
  turn : Color,
  drawDetector : Int }
 


nextTurn : Color -> Color
nextTurn c =
 case c of
  White -> Black
  Black -> White

----- Setup ------

--Converts from algebraic notation to the array index used by the board struct.
--The indices are numbered from a1 (bottom left square) = 0 to h8 (top right square) = 63. Input char must be lower case.
algebraicToIndex : (Char, Int) -> Int
algebraicToIndex (file, rank) =
 ((toCode file) - (toCode 'a')) + (rank - 1) * 8

indexToCoords : Int -> (Int, Int)
indexToCoords index = 
 (remainderBy 8 index, index // 8)

coordsToIndex : (Int, Int) -> Int
coordsToIndex (file, rank) =
 if ((file > 7) || (rank > 7) || (rank < 0) || (file < 0)) then -111
 else (file + (8 * rank))

indexToAlgebraic : Int -> (Char, Int)
indexToAlgebraic index =
 let
  (i,j) = indexToCoords index
  charEncoding = (toCode 'a')
 in
 (fromCode (i + charEncoding), j + 1)

-- Places the pieces on their proper files on the first and eighth ranks.
startingPosition : Int -> Piece
startingPosition i =
 case i of
  0 -> Rook
  1 -> Knight
  2 -> Bishop
  3 -> Queen
  4 -> King
  5 -> Bishop
  6 -> Knight
  7 -> Rook
  _ -> Pawn --This shouldnt happen--

-- Decides which piece to put in each square on initialization.
initSquare : Int -> Square
initSquare index =
 let
  (file, rank) = indexToCoords index
 in
 case rank of
  0 -> C { piece = startingPosition file, color = White}
  1 -> C { piece = Pawn, color = White }
  6 -> C { piece = Pawn, color = Black }
  7 -> C { piece = startingPosition file, color = Black}
  _ -> Empty

startBoard = initialize 64 initSquare

-- Collects together the initial locations of all the white pieces into a list.
initWhitePieces : (List Int)
initWhitePieces =
 (List.range 0 15)

-- Collects together the initial locations of all the black pieces into a list.
initBlackPieces : (List Int)
initBlackPieces =
 (List.range 48 63)

startState : State
startState = 
 { board = startBoard,
  whitePieces = initWhitePieces,
  blackPieces = initBlackPieces,
  whiteCanCastleLong = True,
  blackCanCastleLong = True,
  whiteCanCastleShort = True,
  blackCanCastleShort = True,
  enPassantSquare = -100,
  whiteKingSquare = 4,
  blackKingSquare = 60,
  turn = White,
  drawDetector = 0
   }

---- Rules -----

sameDiagonal : Int -> Int -> Bool
sameDiagonal index1 index2 = 
 let
  (i,j) = (indexToCoords index1)
  (k,l) = (indexToCoords index2)
 in
 ((abs (i - k)) == (abs (j - l)))

canAttackSquare : Color -> Int -> State -> Bool
canAttackSquare c index s = 
 let
  lst = case c of
         White -> s.whitePieces
         Black -> s.blackPieces
 in
  List.foldr (\i -> ((||) (legalMove c i index s))) False lst

inCheck : Color -> State -> Bool
inCheck c s = 
 case c of
  White -> canAttackSquare Black s.whiteKingSquare s
  Black -> canAttackSquare White s.blackKingSquare s

noLegalMoves : State -> Bool
noLegalMoves s = 
 let
  pieces = case (s.turn) of
   White -> s.whitePieces
   Black -> s.blackPieces
 in
 (List.foldr (\sq -> ((&&) (List.foldr (\x -> ((&&) (case (makeMove x sq s) of
  Nothing -> True
  _ -> False))) True pieces))) True (List.range 0 63))


numberOfDefenders : Color -> Int -> State -> Int
numberOfDefenders c index s =
 let
  b = s.board
  testState = {s | 
   board = (set index (C {piece = Pawn, color = (nextTurn c)}) b),
    turn = c}
  defenders = case c of
   White -> s.whitePieces
   Black -> s.blackPieces
 in
 (List.foldr (\x -> ((+) (case (makeMove x index testState) of
                       Nothing -> 0
                       _ -> 1))) 0 defenders)

controlsSquare : Int -> State -> Maybe Color
controlsSquare index s =
 if (numberOfDefenders White index s) == (numberOfDefenders Black index s)
  then Nothing else
   if ((numberOfDefenders White index s) > (numberOfDefenders Black index s))
   then (Just White) else (Just Black)

squareOwnershipValue : Int -> Float
squareOwnershipValue index =
 let 
  (i,j) = (indexToCoords index)
  distanceFromCenter = ((abs (((toFloat i) - 3.5)) + (abs ((toFloat j) - 3.5))))
 in
 centerWeight / (distanceFromCenter)


materialScore : Piece -> Float
materialScore p =
 case p of
  Pawn -> 1
  Knight -> 3
  Bishop -> 3
  Rook -> 5
  Queen -> 9
  King -> 1000

totalScore : Color -> State -> List (Int, Int) -> Float
totalScore c s legalMvs =
 let
  pieces = List.map 
   (\x -> 
    (case (get x s.board) of
      Just (C {piece, color}) -> piece
      _ -> Pawn)) 
   (case c of
     White -> s.whitePieces
     Black -> s.blackPieces)
  material = (List.foldr (\x -> ((+) (materialScore x))) 0 pieces)
  position = (List.foldr (\x -> 
   ((+) (squareOwnershipValue (Tuple.second x)))) 0 legalMvs)
  kingSafe = case c of
   White -> case (s.whiteKingSquare) of
             6 -> 1
             2 -> 0.66
             _ -> 0
   Black -> case (s.blackKingSquare) of
             62 -> 1
             58 -> 0.66
             _ -> 0
  in
  position + material + kingSafe

relativeScore : Color -> State -> Float
relativeScore c s  =
 let
  whiteMvs = case s.turn of
   White -> s
   Black ->{s | turn = (nextTurn s.turn)}
  blackMvs = case s.turn of
   White -> {s | turn = (nextTurn s.turn)}
   Black -> s
 in
 (totalScore White s (listOfLegalMoves whiteMvs)) - (totalScore Black s (listOfLegalMoves blackMvs))


listOfLegalMoves : State -> List (Int, Int)
listOfLegalMoves s =
 let
  indices = case s.turn of
   White -> s.whitePieces
   Black -> s.blackPieces
  pieces = List.map (\i -> (i,
   (case (get i s.board) of
    Just (C {piece, color}) -> piece
    _ -> Debug.todo "error"))) indices
 in 
 (List.filter (\(p,q) -> case (makeMove p q s) of
  Nothing -> False
  _ -> True) (listOfCandidateMoves s.turn pieces))

listOfCandidateMoves : Color -> List (Int, Piece) -> List (Int, Int)
listOfCandidateMoves c pieces =
 case pieces of
  (i, p) :: rst ->
   (List.map (\mv -> (i, mv)) (case p of
    Pawn -> (possiblePawnMoves c i)
    Knight -> possibleKnightMoves i
    Bishop -> possibleBishopMoves i
    Rook -> possibleRookMoves i
    King -> possibleKingMoves i
    Queen -> possibleQueenMoves i)) ++ (listOfCandidateMoves c rst)
  [] -> []

possiblePawnMoves : Color -> Int -> List Int
possiblePawnMoves c sq =
 let
  (i,j) = indexToCoords sq
 in
 case c of
  White -> (if (j == 1) then [coordsToIndex (i,j+2), coordsToIndex (i,j+1), coordsToIndex (i+1,j+1), coordsToIndex (i-1,j+1)]
   else [coordsToIndex (i,j+1), coordsToIndex (i+1,j+1), coordsToIndex (i-1,j+1)])
  Black -> (if (j == 6) then [coordsToIndex (i,j-2),coordsToIndex (i,j-1), coordsToIndex (i-1,j-1), coordsToIndex (i+1,j-1)]
   else [coordsToIndex (i,j-1), coordsToIndex (i-1,j-1), coordsToIndex (i+1,j-1)])

possibleKingMoves : Int -> List Int
possibleKingMoves sq =
 let
  (i,j) = indexToCoords sq
 in
 if ((j == 0) || (j == 7))
 then  [coordsToIndex (i+2, j), coordsToIndex (i-2, j), coordsToIndex (i+1,j), coordsToIndex (i-1,j), coordsToIndex (i+1,j+1), coordsToIndex(i+1,j-1), coordsToIndex (i,j+1), coordsToIndex (i,j-1),coordsToIndex (i-1,j-1), coordsToIndex (i-1,j+1)]
 else [coordsToIndex (i+1,j), coordsToIndex (i-1,j), coordsToIndex (i+1,j+1), coordsToIndex(i+1,j-1), coordsToIndex (i,j+1), coordsToIndex (i,j-1),coordsToIndex (i-1,j-1), coordsToIndex (i-1,j+1)]

possibleRookMoves : Int -> List Int
possibleRookMoves sq =
 let 
  (i,j) = indexToCoords sq
 in
 (List.map (\x -> (coordsToIndex (x, j))) (List.range 0 7)) ++ (List.map (\x -> (coordsToIndex (i, x))) (List.range 0 7))

findDiagonal : (Int, Int) -> (Int, Int) -> List Int
findDiagonal (i,j) (k,l) = 
 let 
  newSquare = coordsToIndex (i+k, j+l)
 in
 if ((0 > newSquare) || (64 < newSquare))
  then []
  else newSquare :: (findDiagonal (i+k, j+l) (k,l))

possibleKnightMoves : Int -> List Int
possibleKnightMoves sq =
 let
  (i,j) = indexToCoords sq
 in
 [coordsToIndex (i+2,j+1), coordsToIndex (i+1,j+2),
  coordsToIndex (i+2,j-1), coordsToIndex(i+1,j-2),
  coordsToIndex (i-2,j+1), coordsToIndex (i-1,j+2),
  coordsToIndex (i-2,j-1), coordsToIndex (i-1,j-2)]

possibleBishopMoves : Int -> List Int
possibleBishopMoves sq =
 let
  (i,j) = indexToCoords sq
  topRightDiagonal = (1,1)
  botRightDiagonal = (-1,1)
  topLeftDiagonal = (1,-1)
  botLeftDiagonal = (-1,-1)
 in
 (findDiagonal (i,j) topRightDiagonal) ++
  (findDiagonal (i,j) botRightDiagonal) ++
  (findDiagonal (i,j) topLeftDiagonal) ++
  (findDiagonal (i,j) botLeftDiagonal) 

possibleQueenMoves : Int -> List Int
possibleQueenMoves sq =
 (possibleBishopMoves sq) ++ (possibleRookMoves sq)

findBestScore : List ((Int, Int), Float) -> ((Int, Int), Float) -> (Int, Int)
findBestScore lst (currBest, currBestScore) =
 case lst of
  (s,f) :: rst -> if (f > currBestScore)
   then findBestScore rst (s,f)
   else findBestScore rst (currBest, currBestScore)
  [] -> currBest

findMoveAI : State -> Int -> State
findMoveAI s depth =
 let 
  maximizer = (s.turn == White)
  (mv, tg) = if maximizer then
    (findBestScore (List.map (\(m,v) -> ((m,v), (alphaBeta maximizer (testMove m v s) -2000.0 2000.0 depth))) (listOfLegalMoves s))
    ((-100,-100), -3000))
   else
    (findBestScore (List.map (\(m,v) -> ((m,v), -1 * (alphaBeta maximizer (testMove m v s) -2000.0 2000.0 depth))) (listOfLegalMoves s))
    ((-100, -100), -3000))
 in
 testMove mv tg s
 

-- adapted from Wikipedia's pseudocode for alpha-beta pruning--
alphaBeta : Bool -> State -> Float -> Float -> Int -> Float
alphaBeta maximizer s alpha beta depth =
 if (depth == 0) then (relativeScore (if maximizer then Black else White) s) else
 if maximizer then 
  let
   value = -2000.0
  in
   forEachMaxChild value s (listOfLegalMoves s) alpha beta depth
 else
  let
   value = 2000.0
  in
   forEachMinChild value s (listOfLegalMoves s) alpha beta depth


forEachMaxChild : Float -> State -> List (Int, Int) -> Float -> Float -> Int -> Float
forEachMaxChild value s children alpha beta depth =
 case children of
  (m,v) :: rst -> 
   let
    ab = (alphaBeta False (testMove m v s) alpha beta (depth - 1))
    newValue = if (value > ab) then value else ab
    newAlpha = if (alpha > newValue) then alpha else newValue
   in
   if (alpha >= beta) then newValue else (forEachMaxChild newValue s rst newAlpha beta depth)
  [] -> value


forEachMinChild : Float -> State -> List (Int, Int) -> Float -> Float -> Int -> Float
forEachMinChild value s children alpha beta depth =
 case children of
  (m,v) :: rst -> 
   let
    ab = (alphaBeta True  (testMove m v s) alpha beta (depth - 1))
    newValue = if (value < ab) then value else ab
    newBeta = if (beta < newValue) then beta else newValue
   in
   if (beta <= alpha) then newValue else (forEachMinChild newValue s rst alpha newBeta depth)
  [] -> value


pawnMove : Color -> Int -> Int -> Board -> Int -> Bool
pawnMove c move target b ep = 
 let
  (i, j) = indexToCoords move
  (k, l) = indexToCoords target
 in
 case (get target b) of
  Nothing -> False
  Just Empty ->
   if (i == k) && ((abs (j - l)) <= 2)
    then --Pawn moving normally--
     case (c, j, l) of
      (White, 1, 3) ->
       case (get (coordsToIndex (i, 2)) b) of
        Just Empty -> True
        _ -> False
      (Black, 6, 4) ->
       case (get (coordsToIndex (i, 5)) b) of
        Just Empty -> True
        _ -> False
      (White, _, _) ->
       (l == j + 1)
      (Black, _, _) ->
       (l == j - 1)
    else --Pawn possibly capturing en passant--
     (isEnPassant c move target ep b)
  Just (C {piece, color}) ->
   if (color == c) then False else
    (
     (abs (i - k)) == 1) && (case c of 
     White -> (l == (j + 1))
     Black -> (l == (j - 1)))



knightMove : Color -> Int -> Int -> Board -> Bool
knightMove c move target b =
 let
  (i,j) = indexToCoords move
  (k, l) = indexToCoords target
  m = (abs (i-k), abs (j-l))
 in
 case (get target b) of
  Nothing -> False
  Just (C {piece, color}) ->
   ((m == (1, 2)) || (m == (2, 1))) && (color == (nextTurn c))
  Just Empty -> ((m == (1, 2)) || (m == (2, 1)))

--Makes sure that no piece blocks the bishop's movement on its way to a move.
nothingInTheWay : (Int, Int) -> Int -> Int -> Board -> Bool
nothingInTheWay (fileDirection, rankDirection) move target b =
 if (move == target) then True else
 case (get move b) of
  Nothing -> False
  Just Empty -> 
   let
    (i,j) = (indexToCoords move)
    newSquare = (coordsToIndex ((i + fileDirection), (j + rankDirection)))
   in
    (nothingInTheWay (fileDirection, rankDirection) newSquare target b)
  Just _ -> False

bishopMove : Color -> Int -> Int -> Board -> Bool
bishopMove c move target b =
 let
    (i, j) = (indexToCoords move)
    (k, l) = (indexToCoords target)
    (m, n) = ((abs (k - i)) // (k - i), (abs (l - j)) // (l - j))
 in
 case (get target b) of
  Nothing -> False
  Just Empty ->
   (sameDiagonal move target) && (nothingInTheWay (m, n) (coordsToIndex (i + m, j + n)) target b)
  Just (C {piece, color}) -> if (color == c) then False else
   (sameDiagonal move target) && (nothingInTheWay (m, n) (coordsToIndex (i + m, j + n)) target b)

rookMove : Color -> Int -> Int -> Board -> Bool
rookMove c move target b = 
 let
  (i, j) = (indexToCoords move)
  (k, l) = (indexToCoords target)
  (m, n) = ((abs (k - i)) // (k - i), (abs (l - j)) // (l - j))
  sameFile = ((i - k) == 0)
  sameRank = ((j - l) == 0)
 in
 case (get target b) of
  Nothing -> False
  Just Empty ->
   ((sameFile || sameRank) && (nothingInTheWay (m, n) (coordsToIndex (i + m, j + n)) target b))
  Just (C {piece, color}) -> if (color == c) then False else
   ((sameFile || sameRank) && (nothingInTheWay (m, n) (coordsToIndex (i + m, j + n)) target b))



queenMove : Color -> Int -> Int -> Board -> Bool
queenMove c move target b = (rookMove c move target b) || (bishopMove c move target b)

kingMove : Color -> Int -> Int -> State -> Bool -> Bool -> Bool
kingMove c move target s canCastleLong canCastleShort = 
 let
  b = s.board
  (i, j) = (indexToCoords move)
  (k, l) = (indexToCoords target)
  (m, n) = (abs (k - i), abs (l - j))
 in
 case (get target b) of
  Nothing -> False
  Just Empty -> (((m <= 1) && (n <= 1))) || 
   case (c, target) of --castling--
    (White, 2) -> canCastleLong && ((get 1 b) == (Just Empty)) && ((get 3 b) == (Just Empty)) &&
      (not ((canAttackSquare Black 2 s) || (canAttackSquare Black 3 s) || (canAttackSquare Black 4 s)))
    (White, 6) -> canCastleShort && ((get 5 b) == (Just Empty)) &&
     (not ((canAttackSquare Black 4 s) || (canAttackSquare Black 5 s) || (canAttackSquare Black 6 s)))
    (Black, 58) -> canCastleLong && ((get 57 b) == (Just Empty)) && ((get 59 b) == (Just Empty)) &&
     (not ((canAttackSquare White 58 s) || (canAttackSquare White 59 s) || (canAttackSquare White 60 s)))
    (Black, 62) -> canCastleShort && ((get 61 b) == (Just Empty)) &&
     (not ((canAttackSquare White 60 s) || (canAttackSquare White 61 s) || (canAttackSquare White 62 s)))
    _ -> False
  Just (C {piece, color}) -> if (color == c) then False else 
   ((m <= 1) && (n <= 1))
  
--Checks if move follows the move rules, but does NOT check to see if a move illegally puts a player in self-check--
legalMove : Color -> Int -> Int -> State -> Bool
legalMove c move target s =
 let
  b = s.board
  ep = s.enPassantSquare
  canCastleLong = if (c == White) then s.whiteCanCastleLong else s.blackCanCastleLong
  canCastleShort = if (c == White) then s.whiteCanCastleShort else s.blackCanCastleShort
 in
 case (get move b) of
  Nothing -> False
  Just Empty -> False
  Just (C { piece, color}) ->
   if (c /= color) then False else
    case piece of
     Pawn -> pawnMove c move target b ep 
     Knight -> knightMove c move target b
     Bishop -> bishopMove c move target b
     Rook -> rookMove c move target b
     Queen -> queenMove c move target b
     King -> kingMove c move target s canCastleLong canCastleShort

isEnPassant : Color -> Int -> Int -> Int -> Board -> Bool
isEnPassant c move target ep b =
 let
  (i,j) = indexToCoords move
  (k,l) = indexToCoords target
 in
 case (get target b) of
  Just Empty ->
   ((target == ep) &&
    (((abs (i - k)) == 1) &&
    (((c == White) && (l == j + 1)) || 
    ((c == Black) && (l == j - 1)))))
  _ -> False

isCastling : Int -> Int -> Board -> Bool
isCastling move target b =
 let
  (i,j) = indexToCoords move
  (k,l) = indexToCoords target
 in
 case (get move b) of
  Just (C {piece, color}) ->
   (piece == King) && ((abs (i-k)) == 2) 
  _ -> False
   
removePieceList : Int -> (List Int) -> (List Int)
removePieceList captured pieces =
 case pieces of
  piece :: rest ->
   if (captured == piece)
    then rest
    else piece :: (removePieceList captured rest)
  [] -> []

replacePieceList : Int -> Int -> (List Int) -> (List Int)
replacePieceList start end list =
 end :: (removePieceList start list)

newEnPassantState : Int -> Int -> State -> State
newEnPassantState move target s =
 let
  capture = case s.turn of
   White -> target - 8
   Black -> target + 8
  newBoard = (set move Empty) <| (set capture Empty) <| (set target (C { piece = Pawn, color = White}) s.board)
  newWhitePieces = case s.turn of
   White -> (replacePieceList move target s.whitePieces)
   Black -> (removePieceList capture s.whitePieces)
  newBlackPieces = case s.turn of
   Black -> (replacePieceList move target s.blackPieces)
   White -> (removePieceList capture s.blackPieces)
 in
  { board = newBoard,
   whitePieces = newWhitePieces,
   blackPieces = newBlackPieces,
   whiteCanCastleLong = s.whiteCanCastleLong,
   blackCanCastleLong = s.blackCanCastleLong,
   whiteCanCastleShort = s.whiteCanCastleShort,
   blackCanCastleShort = s.blackCanCastleShort,
   enPassantSquare = -100,
   whiteKingSquare = s.whiteKingSquare,
   blackKingSquare = s.blackKingSquare,
   turn = nextTurn s.turn,
   drawDetector = 0
    }

isXMove : Piece -> Int -> Board -> Bool
isXMove p move b =
 case (get move b) of
  Just (C {piece, color}) -> (piece == p)
  _ -> False

newCastledState : Int -> Int -> State -> State
newCastledState move target s =
 let
  (i,j) = (indexToCoords move)
  (k,l) = (indexToCoords target)
  shortOrLong = (i - k) -- 2 = long, -2 = short
  oldRookIndex = 
   if (shortOrLong == 2) then (coordsToIndex (0, j)) else (coordsToIndex (7, j))
  newRookIndex =
   if (shortOrLong == 2) then (coordsToIndex (3, j)) else (coordsToIndex (5, j))
  newBoard = 
   if (shortOrLong == 2)
    then ((set move Empty) <| (set target (C { piece = King, color = s.turn}))
     <| (set (coordsToIndex (0, j)) Empty)
      <| (set (coordsToIndex (3, j)) (C { piece = Rook, color = s.turn}) s.board))
    else ((set move Empty) <| (set target (C { piece = King, color = s.turn}))
     <| (set (coordsToIndex (7, j)) Empty)
      <| (set (coordsToIndex (5, j)) (C { piece = Rook, color = s.turn}) s.board))
  newWhitePieces = case s.turn of
   White -> 
    (replacePieceList oldRookIndex newRookIndex s.whitePieces)
   Black -> (s.whitePieces)
  newBlackPieces = case s.turn of
   White -> (s.blackPieces)
   Black -> (replacePieceList oldRookIndex newRookIndex s.blackPieces)
  newWhiteKingSquare = case s.turn of
   White -> target
   Black -> s.whiteKingSquare
  newBlackKingSquare = case s.turn of
   White -> s.blackKingSquare
   Black -> target
  in
   { board = newBoard,
    whitePieces = newWhitePieces,
    blackPieces = newBlackPieces,
    whiteCanCastleShort = (s.turn /= White) && s.whiteCanCastleShort,
    whiteCanCastleLong = (s.turn /= White) && s.whiteCanCastleLong,
    blackCanCastleShort = (s.turn == White) && s.blackCanCastleShort,
    blackCanCastleLong = (s.turn == White) && s.blackCanCastleLong,
    enPassantSquare = -100,
    whiteKingSquare = newWhiteKingSquare,
    blackKingSquare = newBlackKingSquare,
    turn = nextTurn s.turn,
    drawDetector = s.drawDetector + 1 }

newMoveState : Int -> Int -> State -> State
newMoveState move target s =
 let
   (i,j) = indexToCoords move
   (k,l) = indexToCoords target
   movingPiece = case (get move s.board) of
    Nothing -> Debug.todo "Error"
    Just Empty -> Debug.todo "Error"
    Just (C {piece, color}) -> piece
   isCapture = case (get target s.board) of
    Nothing -> Debug.todo "Error"
    Just Empty -> False
    Just _ -> True
   isKingMove = (movingPiece == King)
   isRookMove = (movingPiece == Rook)
   isPawnMove = (movingPiece == Pawn)
   promotePawn = isPawnMove && ((l == 7 || l == 0))
   twoPawnMove = isPawnMove && ((abs (j - l)) == 2)
   newWhiteCanCastleLong = if (s.whiteCanCastleLong && (((isRookMove) && (move == 0)) || isKingMove)) then False else s.whiteCanCastleLong
   newWhiteCanCastleShort = if (s.whiteCanCastleShort && ((isRookMove && (move == 7)) || isKingMove)) then False else s.whiteCanCastleShort
   newBlackCanCastleLong = if (s.blackCanCastleLong && ((isRookMove && (move == 56)) || isKingMove)) then False else s.blackCanCastleLong
   newBlackCanCastleShort = if (s.blackCanCastleShort && ((isRookMove && (move == 63)) || isKingMove)) then False else s.blackCanCastleShort
   newWhitePieces = case s.turn of
    White -> replacePieceList move target s.whitePieces
    Black -> removePieceList target s.whitePieces
   newBlackPieces = case s.turn of
    White -> removePieceList target s.blackPieces
    Black -> replacePieceList move target s.blackPieces
   newBoard = (set move Empty) <| (if promotePawn 
    then (set target (C {piece = Queen, color = s.turn}) s.board)
    else (set target (C {piece = movingPiece, color = s.turn}) s.board))
   newDrawDetector = if (isPawnMove || isCapture) then 0 else (s.drawDetector + 1)
   newTurn = nextTurn s.turn
   newEnPassantSquare = if twoPawnMove then
    case s.turn of
     White -> coordsToIndex (k,l-1)
     Black -> coordsToIndex (k,l+1)
    else -100
   newWhiteKingSquare =
    case s.turn of
     White -> if isKingMove then target else s.whiteKingSquare
     Black -> s.whiteKingSquare
   newBlackKingSquare =
    case s.turn of
     Black -> if isKingMove then target else s.blackKingSquare
     White -> s.blackKingSquare
   in
    { board = newBoard,
    whitePieces = newWhitePieces,
    blackPieces = newBlackPieces,
    whiteCanCastleLong = newWhiteCanCastleLong,
    blackCanCastleLong = newBlackCanCastleLong,
    blackCanCastleShort = newBlackCanCastleShort,
    whiteCanCastleShort = newWhiteCanCastleShort,
    drawDetector = newDrawDetector,
    enPassantSquare = newEnPassantSquare,
    whiteKingSquare = newWhiteKingSquare,
    blackKingSquare = newBlackKingSquare,
    turn = newTurn
   }

tryMove : Int -> Int -> State -> Maybe State
tryMove move target s =
 if (legalMove s.turn move target s) then
  --If capturing en passant--
  if (isEnPassant s.turn move target s.enPassantSquare s.board) then
   Just (newEnPassantState move target s)
   --If Castling--
 else if (isCastling move target s.board) then 
  Just (newCastledState move target s)
 --Other cases of moves/captures--
 else Just (newMoveState move target s)
 --if move is illegal, excluding checking for players putting themselves in self-check--
  else Nothing 
     

makeMove : Int -> Int -> State -> Maybe State
makeMove move target s =
 case (tryMove move target s) of
  Nothing -> Nothing
  Just newState -> if (inCheck s.turn newState)
   then Nothing
   else Just newState

testMove : Int -> Int -> State ->  State
testMove move target s =

 case (tryMove move target s) of
  Nothing -> Debug.todo "error"
  Just newState -> if (inCheck s.turn newState)
   then Debug.todo "error"
   else newState
