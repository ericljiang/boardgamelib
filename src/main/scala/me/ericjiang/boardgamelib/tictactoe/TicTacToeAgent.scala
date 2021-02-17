package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.ai.CachedAlphaBetaAgent

class TicTacToeAgent(player: Player) extends CachedAlphaBetaAgent[TicTacToeState](
  depth = 10,
  heuristic = (state: TicTacToeState) =>
    state.winner match {
    case Some(p) if p == player => 1
    case Some(p) if p != player => -1
    case _ => 0
  })
