package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.ai.{AlphaBetaAgent, AlphaBetaCaching}

class TicTacToeAgent(player: Player) extends AlphaBetaAgent[TicTacToeState](
  depth = 10,
  heuristic = (state: TicTacToeState) =>
    state.winner match {
    case Some(p) if p == player => 1
    case Some(p) if p != player => -1
    case _ => 0
  }) with AlphaBetaCaching[TicTacToeState]
