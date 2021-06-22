module Interface exposing (main)
import Game exposing (..)
import Html exposing (..)
import Html.Events as Events
import Browser
import Browser.Events as BEvents
import Html.Attributes as Attr
import Array exposing (..)
import Json.Decode as Decode
import String
import Html5.DragDrop as DragDrop

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type GameStatus = Checkmate | Draw | Stalemate | Active
type alias Model =
  { state : State,
    dragDrop : DragDrop.Model Int Int,
    status : GameStatus,
    orientation : Bool
  }

type Msg = Reset | DragDropMsg (DragDrop.Msg Int Int) | Noop | SwitchSides | ComputerMove

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

initModel : Model
initModel =
  {state =  Game.startState,
   dragDrop = DragDrop.init,
   status = Active,
   orientation = True
   }

subscriptions : Model -> Sub Msg
subscriptions model =
 Sub.batch
   [BEvents.onKeyDown
    (Decode.map 
     (\key -> (case key of
      "Escape" -> Reset
      "z" -> SwitchSides
      "x" -> ComputerMove
      _ -> Noop))
     (Decode.field "key" Decode.string))]

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
   Noop -> (model, Cmd.none)
   Reset -> (initModel, Cmd.none)
   SwitchSides -> ({model |
                    orientation = (not (model.orientation))}, Cmd.none)
   DragDropMsg m -> 
    let 
     (newModel, result) = DragDrop.update m model.dragDrop
     newState = (case result of
           Nothing -> model.state
           Just (move, target, _) -> 
            case (makeMove move target model.state) of
             Nothing -> model.state
             Just s -> s)
     newStatus = 
      if (noLegalMoves newState) then 
       if (inCheck newState.turn newState) 
        then Checkmate
        else Stalemate
      else if (newState.drawDetector >= 100)
       then Draw
       else Active
    in 
     ({model
       | dragDrop = newModel,
         state = newState,
         status = newStatus,
         orientation = model.orientation
       }, Cmd.none)
   ComputerMove -> ({model | 
                    state = (findMoveAI model.state Game.searchDepth)}, Cmd.none)



uncurry : (a -> b -> c) -> ((a, b) -> c)
uncurry f = (\p -> (f (Tuple.first p) (Tuple.second p)))


pieceText : Piece -> Color -> Int -> Html Msg
pieceText piece color id =
 let
  divText =
   case (piece, color) of
    (Pawn, White) -> "♙"
    (Knight, White) -> "♘"
    (Bishop, White) -> "♗"
    (Rook, White) -> "♖"
    (King, White) -> "♔"
    (Queen, White) -> "♕"
    (Pawn, Black) -> "♟︎"
    (Knight, Black) -> "♞"
    (Bishop, Black) -> "♝"
    (Rook, Black) -> "♜"
    (King, Black) -> "♚"
    (Queen, Black) -> "♛"
 in
 Html.div ((DragDrop.draggable DragDropMsg id) ++ (DragDrop.droppable DragDropMsg id)) [Html.text (divText)]

viewSquare : Square -> Bool -> Int -> Html Msg
viewSquare sq shade index =
 --Shade == True means light square, Shade == False means dark square--
 let 
   squareStyle = if shade
    then [("float", "right"),
     ("height", "50px"),
     ("width", "50px"),
     ("backgroundColor", "#FFF0E5"),
     ("font-size", "40px"),
     ("user-select", "none"),
     ("text-align", "center"),
     ("line-height", "50px")]
    else
     [("float", "right"),
     ("height", "50px"),
     ("width", "50px"),
     ("backgroundColor", "#966948"),
     ("font-size", "40px"),
     ("user-select", "none"),
     ("text-align", "center"),
     ("line-height", "50px")]
 in
 case sq of
  (C {piece, color}) -> (Html.div (List.map (uncurry Attr.style) squareStyle) [(pieceText piece color index)])
  Empty -> (Html.div ((DragDrop.droppable DragDropMsg index) ++ (List.map (uncurry Attr.style) squareStyle)) [])

colorMap : (a -> Bool -> Int -> b) -> Bool -> Int -> Bool -> List a -> List b
colorMap f b i orientation list =
 let nb = (not b) in
 case list of
  x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs -> 
   if orientation then
   (f x1 b (63 - 8*i)) :: (f x2 nb (62 - 8*i)) :: (f x3 b (61 - 8*i)) :: (f x4 nb (60 - 8*i)) :: (f x5 b (59 - 8*i)) :: (f x6 nb (58 - 8*i)) :: (f x7 b (57 - 8*i)) :: (f x8 nb (56 - 8*i)) :: (colorMap f nb (i + 1) orientation xs)
   else 
   (f x1 b (8*i)) :: (f x2 nb (1 + 8*i)) :: (f x3 b (2 + 8*i)) :: (f x4 nb (3 + 8*i)) :: (f x5 b (4 + 8*i)) :: (f x6 nb (5 + 8*i)) :: (f x7 b (6 + 8*i)) :: (f x8 nb (7 + 8*i)) :: (colorMap f nb (i + 1) orientation xs)
  _ -> []

view : Model -> Html Msg
view model =
 let
  txt = [("margin", "20px")]
  boardStyle = [("position", "fixed"),
   ("width", "400px"),
   ("height", "400px"),
   ("top", "10vh"),
   ("left", "33vw")]
  statusMsgStyle = [("text-align", "center")]
  statusMessage = 
   case model.status of
    Active -> 
     case model.state.turn of 
      White -> if (inCheck White model.state) then 
       "White to move. White is in check!" else
       "White to move."
      Black -> if (inCheck Black model.state) then 
        "Black to move. Black is in check!" else
        "Black to move."
    Checkmate -> 
     case model.state.turn of 
      Black -> "Checkmate. White wins!"
      White -> "Checkmate. Black wins!"
    Draw -> "Nothing has happened for 50 turns. It is a draw."
    Stalemate -> "It is a draw by stalemate as there are no legal moves!"
 in
 (Html.div
  (List.map (uncurry Attr.style) txt)
  [(Html.p [] [Html.text "Drag-and-drop pieces to move."]), 
  (Html.p [] [Html.text "Press ESC to reset the board."]), 
   (Html.p [] [Html.text "Press X to have the computer play a move."]),
   (Html.p [] [Html.text "Press Z to flip the board around."]),
   (Html.div
     (List.map (uncurry Attr.style) boardStyle)
     ((case model.orientation of
     True -> (colorMap (\x -> viewSquare x) False 0 True (List.reverse (Array.toList model.state.board)))
     False -> (colorMap (\x -> viewSquare x) False 0 False (Array.toList model.state.board))) ++
      [Html.p (List.map (uncurry Attr.style) statusMsgStyle) [Html.text statusMessage]]))])

