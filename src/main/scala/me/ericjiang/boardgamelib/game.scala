package me.ericjiang.boardgamelib

import scala.util.Try

trait Game[S <: State[S]] {
  def initialState: S
}

trait State[S <: State[S]] {
  def availableActions: Set[Action[S]]
}

trait Action[S <: State[S]] {
  def execute(state: S): ActionResult[S]
  def validate(state: S): Boolean = Try(execute(state)).isSuccess
}

case class ActionResult[S <: State[S]](state: S, events: Seq[Event], availableActions: Set[Action[S]])

trait Event
