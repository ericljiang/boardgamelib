package me.ericjiang.boardgamelib

import shapeless.HMap

import scala.collection.mutable

trait Game[S <: State] {

  protected class RuleRelation[K, V]

  protected implicit def allowed[A <: Action, R <: Rule[A, S]]: RuleRelation[Class[A], R] =
    new RuleRelation[Class[A], R]

  protected def rules: HMap[RuleRelation]

  def initialState: S

  def initialAvailableActions: mutable.MultiMap[Class[_ <: Action], Action]

  def submitAction(actionClass: Class[_ <: Action], action: Action, state: S): ActionResult[S] = {
    val rule: Rule[Action, S] = rules // compiler infers V as Nothing
      .get(actionClass)
      .getOrElse(throw new IllegalArgumentException)
    rule(action, state)
  }
}

trait Action
trait State
trait Event
trait Rule[A <: Action, S <: State] {
  def apply(action: A, state: S): ActionResult[S]
}
