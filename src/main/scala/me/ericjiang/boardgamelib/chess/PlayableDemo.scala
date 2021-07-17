package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.{Action, Event}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object PlayableDemo extends App {
  val input = StdIn.readLine("Play as which side? [w/b] > ")
  val (player, agentSide) = input.toLowerCase match {
    case "w" => (White, Black)
    case "b" => (Black, White)
    case _ => throw new IllegalArgumentException
  }
  val agent = new ChessAgent(agentSide, 4)

  var state = ChessGame.initialState
  var availableActions = state.availableActions
  val events = ListBuffer.empty[Event]
  val actions = ListBuffer.empty[Action[ChessState]]

  while (availableActions.nonEmpty) {
    val action = if (state.activePlayer == player) {
      MovePieceAction.parse(StdIn.readLine("Enter a move > "), player)
    } else {
      val start = System.nanoTime
      val action = agent.chooseAction(state)
      val end = System.nanoTime
      val time = (end - start) / 1e9d
      println(s"$action (${time}s)")
      action
    }

    val result = action.execute(state)
    state = result.state
    events ++= result.events
    actions += action
    availableActions = result.availableActions

    println(state)
  }

  println(MovePieceAction.parse(StdIn.readLine("Enter a move > "), player))
}
