package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

/**
 * An agent that uses a heuristic function to choose an [[Action]] to take in a 2-player game by minimizing the possible
 * loss for a worst case (maximum loss) scenario.
 * @param depth number of moves ahead to analyze
 * @param heuristic function that assigns a value to a game state
 * @tparam S type of [[State]]
 */
class MinimaxAgent[S <: State[S]](depth: Int, heuristic: S => Double) extends Agent[S] {

  require(depth > 0)

  def chooseAction(state: S): Action[S] =
    state.availableActions
//      .filter(_.validate(state))
      .maxBy(action => minimax(action.execute(state).state, depth - 1, maximizingPlayer = false))

  private def minimax(state: S, depth: Int, maximizingPlayer: Boolean): Double = {
    val availableActions = state.availableActions
    //      .filter(_.validate(result.state))
    val value = if (depth == 0 || availableActions.isEmpty) {
      heuristic(state)
    } else if (maximizingPlayer) {
      availableActions
        .map(action => minimax(action.execute(state).state, depth - 1, maximizingPlayer = false))
        .max
    } else {
      availableActions
        .map(action => minimax(action.execute(state).state, depth - 1, maximizingPlayer = true))
        .min
    }
    value
  }
}
