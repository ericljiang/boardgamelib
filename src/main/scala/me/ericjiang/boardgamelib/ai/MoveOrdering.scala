package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.util.ScalaUtils.FoldWhile
import me.ericjiang.boardgamelib.{Action, State}

import scala.Double.NegativeInfinity
import scala.collection.mutable

trait MoveOrdering[S <: State[S]] extends AlphaBetaAgent[S] {

  val killerMoves: mutable.Map[Int, Action[S]] = mutable.Map.empty

  override abstract def maximumScore(availableActions: Seq[Action[S]], state: S, depth: Int, α: Double, β: Double): Double = {
    val (value, _, killerMove) = availableActions.sortBy(ordering(_, state, depth))
      .foldWhile((NegativeInfinity, α, null: Action[S])) { case (_, α, _) => α < β } {
        case ((value, α, _), action) => (
          Math.max(value, alphaBeta(action.execute(state).state, depth - 1, α, β, maximizingPlayer = false)),
          Math.max(α, value),
          action
        )
      }
    killerMoves(depth) = killerMove
    value
  }

  protected def ordering(action: Action[S], state: S, depth: Int): Int =
    if (killerMoves.get(depth).contains(action)) -1 else 0
}
