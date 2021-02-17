package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, Game, State}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * An agent that uses a heuristic function to choose an [[Action]] to take in a 2-player game by minimizing the possible
 * loss for a worst case (maximum loss) scenario.
 * @param game [[Game]] instance
 * @param depth number of moves ahead to analyze
 * @param heuristic function that assigns a value to a game state
 * @tparam S type of [[State]]
 * @tparam G type of [[Game]]
 */
class MinimaxAgent[S <: State, G <: Game[S]](game: G, depth: Int, heuristic: S => Double) extends Agent[S] {

  require(depth > 0)

  def chooseAction(availableActions: mutable.MultiMap[Class[_ <: Action], Action], state: S): (Class[_ <: Action], Action) =
    availableActions
      .flatMap { case (actionClass, actions) => actions.map((actionClass, _)) }
//      .filter { case (actionClass, action) => validateAction(actionClass, action, state) }
      .maxBy { case (actionClass, action) => minimax(actionClass, action, state, depth - 1, maximizingPlayer = false)}

  private def minimax(actionClass: Class[_ <: Action], action: Action, state: S, depth: Int, maximizingPlayer: Boolean): Double = {
    val result = game.submitAction(actionClass, action, state)
    val availableActions = result.availableActions
      .flatMap { case (actionClass, actions) => actions.map((actionClass, _)) }
//      .filter { case (actionClass, action) => validateAction(actionClass, action, result.state) }
    val value = if (depth == 0 || availableActions.isEmpty) {
      heuristic(result.state)
    } else if (maximizingPlayer) {
      availableActions
        .map { case (actionClass, action) => minimax(actionClass, action, result.state, depth - 1, maximizingPlayer = false) }
        .max
    } else {
      availableActions
        .map { case (actionClass, action) => minimax(actionClass, action, result.state, depth - 1, maximizingPlayer = true) }
        .min
    }
    value
  }

  private def validateAction(actionClass: Class[_ <: Action], action: Action, state: S): Boolean =
    Try(game.submitAction(actionClass, action, state)) match {
      case Success(_) => true
      case Failure(_) => false
    }
}
