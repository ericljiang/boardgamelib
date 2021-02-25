package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.chess.ChessFiles._
import me.ericjiang.boardgamelib.testutil.BaseTest

class ChessAgentIntegrationTest extends BaseTest {
  "A ChessAgent" should "move out of check" in {
    val agent = new ChessAgent(Black, 2)
    val setupMoves = Seq(
      MovePieceAction(Pawn(White), (e, 2), (e, 4)),
      MovePieceAction(Pawn(Black), (d, 7), (d, 6)),
      MovePieceAction(Bishop(White), (f, 1), (b, 5))
    )
    val testState = setupMoves.foldLeft(ChessGame.initialState) { (state, move) => move.execute(state).state }
    val chosenAction = agent.chooseAction(testState)
//    val chosenAction = agent.chooseAction(Set(
//      MovePieceAction(King(Black), (e, 8), (d, 7)),
//      MovePieceAction(Bishop(Black), (c, 8), (d, 7))
//    ), testState)
    println(s"Chose $chosenAction")
    println(chosenAction.execute(testState).state)
//    assert(chosenAction.execute(testState).state.board.contains((d, 7)))
  }
}
