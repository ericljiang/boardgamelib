package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, Game, State}

import scala.collection.mutable

class CachedAlphaBetaAgent[S <: State, G <: Game[S]](game: G, depth: Int, heuristic: S => Double)
  extends AlphaBetaAgent[S, G](game, depth, heuristic) {

  val cache: mutable.Map[(Class[_ <: Action], Action, S, Int, Double, Double, Boolean), Double] = mutable.Map.empty
//  var cacheHits = 0

  override protected def alphaBeta(actionClass: Class[_ <: Action], action: Action, state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
//    cacheHits += 1
    cache.getOrElse((actionClass, action, state, depth, α, β, maximizingPlayer), {
//      cacheHits -= 1
      val value = super.alphaBeta(actionClass, action, state, depth, α, β, maximizingPlayer)
      cache((actionClass, action, state, depth, α, β, maximizingPlayer)) = value
      value
    })
  }
}
