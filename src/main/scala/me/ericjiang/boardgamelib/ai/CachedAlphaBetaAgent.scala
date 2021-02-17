package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

import scala.collection.mutable

class CachedAlphaBetaAgent[S <: State[S]](depth: Int, heuristic: S => Double)
  extends AlphaBetaAgent[S](depth, heuristic) {

  val cache: mutable.Map[(Action[S], S, Int, Double, Double, Boolean), Double] = mutable.Map.empty
//  var cacheHits = 0

  override protected def alphaBeta(action: Action[S], state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
//    cacheHits += 1
    cache.getOrElse((action, state, depth, α, β, maximizingPlayer), {
//      cacheHits -= 1
      val value = super.alphaBeta(action, state, depth, α, β, maximizingPlayer)
      cache((action, state, depth, α, β, maximizingPlayer)) = value
      value
    })
  }
}
