package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

import scala.Double.{NegativeInfinity => -∞, PositiveInfinity => ∞}

class AlphaBetaAgent[S <: State[S]](depth: Int, heuristic: S => Double) extends Agent[S] {

  require(depth > 0)
  require(heuristic != null)

  override def chooseAction(state: S): Action[S] =
    state.availableActions.maxBy(action => alphaBeta(
      state = action.execute(state).state,
      depth = depth - 1,
      α = -∞,
      β = ∞,
      maximizingPlayer = false))

  protected def alphaBeta(state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    val availableActions = state.availableActions
    if (depth == 0 || availableActions.isEmpty) {
      heuristic(state)
    } else if (maximizingPlayer) {
      val (value, _) = availableActions.foldLeft((-∞, α)) {
        case ((value, α), _) if α >= β => (value, α)
        case ((value, α), action) => (
          Math.max(value, alphaBeta(action.execute(state).state, depth - 1, α, β, maximizingPlayer = false)),
          Math.max(α, value)
        )
      }
      value
    } else {
      val (value, _) = availableActions.foldLeft((∞, β)) {
        case ((value, β), _) if β <= α => (value, β)
        case ((value, β), action) => (
          Math.min(value, alphaBeta(action.execute(state).state, depth - 1, α, β, maximizingPlayer = true)),
          Math.min(β, value)
        )
      }
      value
    }
  }
}
