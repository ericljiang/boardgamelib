package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, State}

trait Agent[S <: State[S]] {
  def chooseAction(availableActions: Set[Action[S]], state: S): Action[S]
}
