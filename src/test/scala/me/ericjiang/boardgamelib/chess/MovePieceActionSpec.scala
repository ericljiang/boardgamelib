package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.chess.ChessFiles._
import me.ericjiang.boardgamelib.testutil.BaseTest

class MovePieceActionSpec extends BaseTest {
  "A move that captures a King" should "produce a state with a winner" in {
    val state = ChessState(
      activePlayer = White,
      board = Map(
        (d, 1) -> Queen(White),
        (e, 1) -> King(White),
        (d, 8) -> King(Black)),
      winner = None)
    val move = MovePieceAction(piece = Queen(White), origin = (4, 1), destination = (4, 8))
    assertResult(Some(White)) { move.execute(state).state.winner }
  }

  "A move that captures a non-King piece" should "produce a state with no winner" in {
    val state = ChessState(
      activePlayer = White,
      board = Map(
        (e, 1) -> King(White),
        (e, 8) -> King(Black),
        (e, 2) -> Pawn(Black)),
      winner = None)
    val move = MovePieceAction(piece = King(White), origin = (e, 1), destination = (e, 2))
    assert(move.execute(state).state.winner.isEmpty)
  }
}
