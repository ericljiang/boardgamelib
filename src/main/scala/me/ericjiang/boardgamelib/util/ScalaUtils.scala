package me.ericjiang.boardgamelib.util

import scala.annotation.tailrec
import scala.collection.mutable

object ScalaUtils {
  def emptyMultiMap[K, V]: mutable.MultiMap[K, V] =
    new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V]

  // https://stackoverflow.com/a/36518788/6497736
  implicit class FoldWhile[T](val items: Iterable[T]) extends AnyVal {
    def foldWhile[A](zero: A)(until: A => Boolean)(op: (A, T) => A): A = {
      @tailrec def loop(acc: A, remaining: Iterable[T]): A =
        if (remaining.isEmpty || !until(acc)) acc else loop(op(acc, remaining.head), remaining.tail)
      loop(zero, items)
    }
  }
}
