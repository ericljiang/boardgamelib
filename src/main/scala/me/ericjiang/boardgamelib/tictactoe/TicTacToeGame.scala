package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.{Action, Game, State}

class TicTacToeGame extends Game[TicTacToeState] {
  override def initialState: TicTacToeState = TicTacToeState(
    activePlayer = X,
    board = Vector.fill[Option[Player]](3, 3)(None))
}

case class TicTacToeState(activePlayer: Player, board: Vector[Vector[Option[Player]]], winner: Option[Player] = None) extends State[TicTacToeState] {
  def renderBoard: String = board
    .map(row => row
      .map {
        case None => " "
        case Some(t) => t.toString
      }
      .mkString(" | "))
    .map(r => s" $r ")
    .mkString("\n---|---|---\n")

  override def availableActions: Seq[Action[TicTacToeState]] =
    if (winner.isDefined) {
      Seq.empty
    } else {
      emptySpaces
        .map { case (r, c) => MarkAction(activePlayer, r, c) }
        .toSeq
    }

  def emptySpaces: Iterable[(Int, Int)] =
    for {
      r <- 0 to 2
      c <- 0 to 2
      if board(r)(c).isEmpty
    } yield (r, c)
}

sealed trait Player
case object X extends Player
case object O extends Player
