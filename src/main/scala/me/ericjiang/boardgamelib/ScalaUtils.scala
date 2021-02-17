package me.ericjiang.boardgamelib

import scala.collection.mutable

object ScalaUtils {
  def emptyMultiMap[K, V]: mutable.MultiMap[K, V] =
    new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V]
}
