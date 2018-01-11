package com.ontheserverside.galaxy

import strawman.collection.IndexedView

import scala.collection.parallel.SeqSplitter
import scala.collection.parallel.immutable.ParSeq
import scala.collection.mutable.ArrayBuffer

class ParIndexedView[T](val delegate: IndexedView[T]) extends ParSeq[T] {
  override def apply(idx: Int): T = delegate(idx)

  override def length: Int = delegate.length

  override def seq: scala.collection.immutable.Seq[T] = {
    delegate.toSeq.toClassic.to[collection.immutable.Seq]
  }

  def splitter: SeqSplitter[T] = new IndexedViewSplitter(0, length)

  class IndexedViewSplitter(
     private[this] var idx: Int,
     private[this] val totalLength: Int
   ) extends SeqSplitter[T] {
    override def hasNext: Boolean = idx < totalLength

    override def next(): T = {
      val v = delegate(idx)
      idx += 1
      v
    }

    override def remaining: Int = totalLength - idx

    override def dup: SeqSplitter[T] = new IndexedViewSplitter(idx, totalLength)

    override def split: Seq[SeqSplitter[T]] = {
      val rem = remaining
      if (rem > 2) {
        psplit(rem / 2, rem - rem / 2)
      } else {
        Seq(this)
      }
    }

    override def psplit(sizes: Int*): Seq[SeqSplitter[T]] = {
      val splitters = new ArrayBuffer[IndexedViewSplitter]
      for (size <- sizes) {
        val next = (idx + size) min totalLength
        splitters += new IndexedViewSplitter(idx, next)
        idx = next
      }
      if (remaining > 0) splitters += new IndexedViewSplitter(idx, totalLength)
      splitters
    }
  }
}
