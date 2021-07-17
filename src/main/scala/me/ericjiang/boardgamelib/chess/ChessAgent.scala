package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.Action
import me.ericjiang.boardgamelib.ai.{AlphaBetaAgent, AlphaBetaCaching, MoveOrdering}

class ChessAgent(player: Player, depth: Int)
  extends AlphaBetaAgent[ChessState](depth = depth, heuristic = ChessAgent.heuristic(player, _))
    with AlphaBetaCaching[ChessState]
    with MoveOrdering[ChessState] {
  override def ordering(action: Action[ChessState], state: ChessState, depth: Int): Int = {
    if (state.board.contains(action.asInstanceOf[MovePieceAction].destination))
      -2
    else
      super.ordering(action, state, depth)
  }
}

object ChessAgent {
  def heuristic(player: Player, state: ChessState): Double = {
    state.winner match {
      case None => state.board.values.map { piece => {
        val value = piece match {
          case Pawn(_) => 1
          case King(_) => 0
          case Queen(_) => 9.5
          case Bishop(_) => 3.33
          case Knight(_) => 3.05
          case Rook(_) => 5.63
        }
        (if (piece.player == player) 1 else -1) * value
      }}.sum
      case Some(p) if p == player => 1000
      case Some(p) if p != player => -1000
    }
  }
}
