package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, Game, State}

import scala.Double.{NegativeInfinity => −∞, PositiveInfinity => ∞}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class AlphaBetaAgent[S <: State, G <: Game[S]](game: G, depth: Int, heuristic: S => Double) extends Agent[S] {

  require(depth > 0)

  def chooseAction(availableActions: mutable.MultiMap[Class[_ <: Action], Action], state: S): (Class[_ <: Action], Action) =
    availableActions
      .flatMap { case (actionClass, actions) => actions.map((actionClass, _)) }
//      .filter { case (actionClass, action) => validateAction(actionClass, action, state) }
      .maxBy { case (actionClass, action) => alphaBeta(actionClass, action, state, depth - 1, −∞, ∞, maximizingPlayer = false)}

  protected def alphaBeta(actionClass: Class[_ <: Action], action: Action, state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    val result = game.submitAction(actionClass, action, state)
    val availableActions = result.availableActions
      .flatMap { case (actionClass, actions) => actions.map((actionClass, _)) }
//      .filter { case (actionClass, action) => validateAction(actionClass, action, result.state) }
    if (depth == 0 || availableActions.isEmpty) {
      heuristic(result.state)
    } else if (maximizingPlayer) {
      val (value, _) = availableActions.foldLeft((−∞, α)) {
        case ((value, α), (actionClass, action)) =>
          if (α >= β) {
            (value, α)
          } else {
            (
              Math.max(value, alphaBeta(actionClass, action, result.state, depth - 1, α, β, maximizingPlayer = false)),
              Math.max(α, value)
            )
          }
      }
      value
    } else {
      val (value, _) = availableActions.foldLeft((∞, β)) {
        case ((value, β), (actionClass, action)) =>
          if (β <= α) {
            (value, β)
          } else {
            (
              Math.min(value, alphaBeta(actionClass, action, result.state, depth - 1, α, β, maximizingPlayer = true)),
              Math.min(β, value)
            )
          }
      }
      value
    }
  }

  private def validateAction(actionClass: Class[_ <: Action], action: Action, state: S): Boolean =
    Try(game.submitAction(actionClass, action, state)) match {
      case Success(_) => true
      case Failure(_) => false
    }
}
