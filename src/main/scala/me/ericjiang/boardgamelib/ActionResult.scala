package me.ericjiang.boardgamelib

import scala.collection.mutable

case class ActionResult[S <: State](state: S, events: Seq[Event], availableActions: mutable.MultiMap[Class[_ <: Action], Action])
