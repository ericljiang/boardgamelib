package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.State

import scala.collection.mutable

class CachedAlphaBetaAgent[S <: State[S]](depth: Int, val heuristic: S => Double)
  extends AlphaBetaAgent[S](depth, heuristic) {

  val cache: mutable.Map[(S, Int, Double, Double, Boolean), Double] = mutable.Map.empty
  var cacheHits = 0

  override protected def alphaBeta(state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    cacheHits += 1
    cache.getOrElse((state, depth, α, β, maximizingPlayer), {
      cacheHits -= 1
      val value = super.alphaBeta(state, depth, α, β, maximizingPlayer)
      cache((state, depth, α, β, maximizingPlayer)) = value
      value
    })
  }
}
