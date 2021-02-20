package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.chess.ChessFiles._
import me.ericjiang.boardgamelib.{Action, ActionResult}

case class MovePieceAction(piece: Piece, origin: (Int, Int), destination: (Int, Int)) extends Action[ChessState] {
  override def execute(state: ChessState): ActionResult[ChessState] = {
    require(state.activePlayer == piece.player)
    require(state.board(origin) == piece)

    val (file, rank) = destination
    require(a <= file && file <= h)
    require(1 <= rank && rank <= 8)

    val winner = state.board.get(destination)
      .map(capturedPiece => {
        require(capturedPiece.player != state.activePlayer)
        capturedPiece
      })
      .filter {
        case King(_) => true
        case _ => false
      }
      .map(_.player)

    val newState = ChessState(
      activePlayer = state.activePlayer match { case White => Black; case Black => White },
      board = state.board - origin + (destination -> piece),
      winner = winner)

    ActionResult(
      state = newState,
      events = Seq.empty,
      availableActions = newState.availableActions)
  }
}
