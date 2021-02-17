package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.{Action, ActionResult, Event}

case class MarkAction(player: Player, row: Int, col: Int) extends Action[TicTacToeState] {
  override def apply(state: TicTacToeState): ActionResult[TicTacToeState] = {
    require(state.activePlayer == player)
    require(0 <= row && row < 3)
    require(0 <= col && col < 3)
    require(state.board(row)(col).isEmpty)
    val newRow = state.board(row).updated(col, Some(player))
    val newBoard = state.board.updated(row, newRow)
    val winner = if (
      newRow.forall(_.contains(state.activePlayer)) ||
        newBoard.map(r => r(col)).forall(_.contains(state.activePlayer)) ||
        (for (r <- 0 to 2; c <- 0 to 2 if r == c) yield newBoard(r)(c)).forall(_.contains(state.activePlayer)) ||
        (for (r <- 0 to 2; c <- 0 to 2 if r + c == 2) yield newBoard(r)(c)).forall(_.contains(state.activePlayer))
    ) {
      Some(state.activePlayer)
    } else {
      None
    }
    val newActivePlayer = state.activePlayer match {
      case X => O
      case O => X
    }
    val newState = TicTacToeState(newActivePlayer, newBoard, winner)
    ActionResult(
      state = TicTacToeState(newActivePlayer, newBoard, winner),
      events = Seq(MarkEvent(player, row, col)),
      availableActions = newState.availableActions
    )
  }
}
case class MarkEvent(player: Player, row: Int, col: Int) extends Event
