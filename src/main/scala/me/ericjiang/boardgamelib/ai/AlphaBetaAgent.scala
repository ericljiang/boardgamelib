package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.util.ScalaUtils.FoldWhile
import me.ericjiang.boardgamelib.{Action, State}

import scala.Double.{NegativeInfinity => -∞, PositiveInfinity => ∞}

class AlphaBetaAgent[S <: State[S]](depth: Int, heuristic: S => Double) extends Agent[S] {
  require(depth > 0)

  protected def getAvailableActions(state: S): Seq[Action[S]] = state.availableActions

  override def chooseAction(state: S): Action[S] =
    getAvailableActions(state)
      .maxBy(action => alphaBeta(action.execute(state).state, depth - 1, -∞, ∞, maximizingPlayer = false))

  protected def alphaBeta(state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    val availableActions = getAvailableActions(state)
    if (depth == 0 || availableActions.isEmpty) {
      heuristic(state)
    } else if (maximizingPlayer) {
      maximumScore(availableActions, state, depth, α, β)
    } else {
      minimumScore(availableActions, state, depth, α, β)
    }
  }

  protected def maximumScore(availableActions: Seq[Action[S]], state: S, depth: Int, α: Double, β: Double): Double = {
    availableActions.foldWhile((-∞, α)) { case (_, α) => α < β } {
      case ((value, α), action) =>(
        Math.max(value, alphaBeta(action.execute(state).state, depth - 1, α, β, maximizingPlayer = false)),
        Math.max(α, value)
      )
    }._1
  }

  protected def minimumScore(availableActions: Seq[Action[S]], state: S, depth: Int, α: Double, β: Double): Double = {
    availableActions.foldWhile((∞, β)) { case (_, β) => β > α } {
      case ((value, β), action) => (
        Math.min(value, alphaBeta(action.execute(state).state, depth - 1, α, β, maximizingPlayer = true)),
        Math.min(β, value)
      )
    }._1
  }
}
