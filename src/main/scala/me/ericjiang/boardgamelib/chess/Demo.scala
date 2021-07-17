package me.ericjiang.boardgamelib.chess

import me.ericjiang.boardgamelib.{Action, Event}

import scala.collection.mutable.ListBuffer

object Demo extends App {
  println("chess")
  val agentWhite = new ChessAgent(White, 4)
  val agentBlack = new ChessAgent(Black, 4)

  var state = ChessGame.initialState
  var availableActions = state.availableActions
  val events = ListBuffer.empty[Event]
  val actions = ListBuffer.empty[Action[ChessState]]

  while (availableActions.nonEmpty) {
    val agent = state.activePlayer match {
      case White => agentWhite
      case Black => agentBlack
    }
    val start = System.nanoTime
    val action = agent.chooseAction(state)
    val end = System.nanoTime
    val time = (end - start) / 1e9d
    val result = action.execute(state)
    state = result.state
    events ++= result.events
    actions += action
    availableActions = result.availableActions

    println(s"$action (${time}s)")
    println(agent.invocations)
    println(agent.cacheMisses)
//    println(agent.heuristic(state))
    println(state)
//    println
  }
  println(actions.mkString(" "))
}
