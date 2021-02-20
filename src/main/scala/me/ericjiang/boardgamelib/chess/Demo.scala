package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.Event

import scala.collection.mutable.ListBuffer

object Demo extends App {
  println("chess")
  val game = new ChessGame
  val agentWhite = new ChessAgent(White, 4)
  val agentBlack = new ChessAgent(Black, 1)

  var state = game.initialState
  var availableActions = state.availableActions
  val events = ListBuffer.empty[Event]

  while (availableActions.nonEmpty) {
    val agent = state.activePlayer match {
      case White => agentWhite
      case Black => agentBlack
    }
    val start = System.nanoTime
    val action = agent.chooseAction(availableActions, state)
    val end = System.nanoTime
    val time = (end - start) / 1e9d
    val result = action.execute(state)
    state = result.state
    availableActions = result.availableActions
    events ++= result.events
    println(s"$action (${time}s)")
    println(agent.heuristic(state))
    println(state)
//    println
  }
}
