package me.ericjiang.boardgamelib.tictactoe

import me.ericjiang.boardgamelib.Event

import scala.collection.mutable.ListBuffer

object Demo extends App {
  val game = new TicTacToeGame
  val agentX = new TicTacToeAgent(X)
  val agentO = new TicTacToeAgent(O)

  var state = game.initialState
  var availableActions = state.availableActions
  val events = ListBuffer.empty[Event]

  val start = System.nanoTime

  while (availableActions.nonEmpty) {
    val agent = state.activePlayer match {
      case X => agentX
      case O => agentO
    }
    val action = agent.chooseAction(availableActions, state)
    val result = action.execute(state)
    state = result.state
    availableActions = result.availableActions
    events ++= result.events
    println(state.renderBoard)
    println
  }

  val end = System.nanoTime
  println((end - start) / 1e9d)
  //  println(agentX.cacheHits)
}
