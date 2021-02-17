package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

import scala.collection.mutable

trait Agent[S <: State] {
  def chooseAction(availableActions: mutable.MultiMap[Class[_ <: Action], Action], state: S): (Class[_ <: Action], Action)
}
