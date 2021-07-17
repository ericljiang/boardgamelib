package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.State

import scala.collection.mutable

trait AlphaBetaCaching[S <: State[S]] extends AlphaBetaAgent[S] {

  val cache: mutable.Map[(S, Int, Double, Double, Boolean), Double] = mutable.Map.empty
  var invocations = 0
  var cacheMisses = 0

  override abstract def alphaBeta(state: S, depth: Int, α: Double, β: Double, maximizingPlayer: Boolean): Double = {
    invocations += 1
    cache.getOrElse((state, depth, α, β, maximizingPlayer), {
      cacheMisses += 1
      val value = super.alphaBeta(state, depth, α, β, maximizingPlayer)
      cache((state, depth, α, β, maximizingPlayer)) = value
      value
    })
  }
}
