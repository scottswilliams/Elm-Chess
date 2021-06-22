Elm Chess
by Scott Williams

This chess program is a functional, interactive simulator of the game chess, with support for AI moves.

Features
---
The game functions as you would expect, implementing almost all the standard rules of chess, including preventing the user from making illegal moves, identifying and displaying check, castling, en passant, and detecting stalemate, checkmate, and draw by 50 move rule detection. Pawns are promoted to queens when they reach the eighth rank. The only features not simulated by the program that exist in real-life chess are underpromotion (the ability to promote pawns to knights, bishops, and rooks), draws by repetition, resignations and draws by agreement.

The game can be played by dragging a piece with the mouse and dropping it into the desired square. The user can move both sides' pieces.

The board interface has features some keyboard commands. The ESCAPE key resets the game to its starting position. The z key rotates the board to the other player's perspective. The x key has the AI play one move for whichever side's turn it is. Pressing the x key multiple times will request multiple AI moves.

The AI uses an alpha-beta pruning algorithm that takes about 4 seconds to make a move, unless there are only a few pieces left. The heuristic is very simple, based on the material value of each side's pieces, plus a positional value to players that makes controlling more central squares more valubale than edge-of-the-board squares. The heuristic also gives a bonus for castling. The heuristic's positional value calculator seems to be responsible for most of the calculation time.

Commentary
---

Elm makes debugging extremely easy. I have never had such a bug-free experience with any programming language. However, performance is the main issue with Elm, which only became important when I coded the AI. The way that the game determines if a move is legal or illegal is the weakest part of the implementation, as it requires making an entire new resulting state from that move and testing whether that state is illegal (a player can never allow their king to be captured, for example, and I am not sure how to determine that without building a new state entirely). 

The board itself is an Array of its squares. I tested it as a List because I was not sure whether I used too many (set ...) functions for the Array to be efficient, but the first move takes about 3.5 seconds with an Array and 5 seconds with a List.

The AI was originally very inefficient, and took about 30 seconds to make its first move using a MinMax algorithm. A Alpha Beta pruning algorithm did not help much either, bringing down the first move to 25 seconds of calculation. After trying many different improvements to the data structures and making the calculations more efficient, I realized that the positional value heuristic was very inefficient, and the main cause for the slow calculation time. Changing the positional value heuristic brought the first-move calculation down to the final 3.5 seconds. My main struggle with Elm was I did not know how to debug performance issues like this, and I could not figure out what was slowing everything down without a lot of trial and error. The heuristic was my last thought on what could be slowing the program down.