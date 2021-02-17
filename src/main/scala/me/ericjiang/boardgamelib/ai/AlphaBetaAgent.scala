package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

import scala.Double.{NegativeInfinity => −∞, PositiveInfinity => ∞}

class AlphaBetaAgent[S <: State[S]](depth: Int, heuristic: S => Double) extends Agent[S] {

  require(depth > 0)

  override def chooseAction(availableActions: Set[Action[S]], state: S): Action[S] =
    availableActions
//      .filter(_.validate(state))
      .maxBy(alphaBeta(_, state, depth - 1, −∞, ∞, maximizingPlayer = false))

  protected def alphaBeta(action: Action[S], state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    val result = action(state)
    val availableActions = result.availableActions
    //      .filter(_.validate(result.state))
    if (depth == 0 || availableActions.isEmpty) {
      heuristic(result.state)
    } else if (maximizingPlayer) {
      val (value, _) = availableActions.foldLeft((−∞, α)) {
        case ((value, α), action) =>
          if (α >= β) {
            (value, α)
          } else {
            (
              Math.max(value, alphaBeta(action, result.state, depth - 1, α, β, maximizingPlayer = false)),
              Math.max(α, value)
            )
          }
      }
      value
    } else {
      val (value, _) = availableActions.foldLeft((∞, β)) {
        case ((value, β), action) =>
          if (β <= α) {
            (value, β)
          } else {
            (
              Math.min(value, alphaBeta(action, result.state, depth - 1, α, β, maximizingPlayer = true)),
              Math.min(β, value)
            )
          }
      }
      value
    }
  }
}
