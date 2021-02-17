package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.ScalaUtils.emptyMultiMap
import me.ericjiang.boardgamelib.{Action, Game, State, tictactoe}
import shapeless.HMap

import scala.collection.mutable

class TicTacToeGame extends Game[TicTacToeState] {

  override def initialState: TicTacToeState = TicTacToeState(
    activePlayer = X,
    board = Vector.fill[Option[Player]](3, 3)(None))

  override def initialAvailableActions: mutable.MultiMap[Class[_ <: Action], Action] =
    initialState.availableActions

  override protected def rules: HMap[RuleRelation] = HMap[RuleRelation](
    classOf[MarkAction] -> MarkRule
  )
}

case class TicTacToeState(activePlayer: Player, board: Vector[Vector[Option[Player]]], winner: Option[Player] = None) extends State {
  def renderBoard: String = board
    .map(row => row
      .map {
        case None => " "
        case Some(t) => t.toString
      }
      .mkString(" | "))
    .map(r => s" $r ")
    .mkString("\n---|---|---\n")

  def availableActions: mutable.MultiMap[Class[_ <: Action], Action] =
    if (winner.isDefined) {
      emptyMultiMap[Class[_ <: Action], Action]
    } else {
      emptySpaces
        .map { case (r, c) => tictactoe.MarkAction(activePlayer, r, c) }
        .foldLeft(emptyMultiMap[Class[_ <: Action], Action]) {
          (map, action) => map.addBinding(classOf[MarkAction], action)
        }
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
