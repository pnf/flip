package com.podsnap.flip
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.{IterableView, SeqView}
import scala.collection.immutable.VectorBuilder
import scala.collection.Iterator
import scala.collection.Iterable
import com.typesafe.scalalogging.slf4j.Logging

class SeqF[A] (l : Seq[()=>A]) {
  def map[B](f : A =>B) = new SeqF(l.map( x => () => f(x())))

  def take(n : Int) = new SeqF(l.take(n))

  // l.zipWithIndex returns sequence of pairs (()=>A,Int)
  // this.zipWithIndex returns sequence of functors yielding pairs (A,Int)
  def zipWithIndex = new SeqF( l.zipWithIndex.map( x => () => (x._1(),x._2) ) )

  def scanLeft[B](b0 : B)(f : (B,A)=>B) : SeqF[B] = {
    val l2 = l.scanLeft( ()=>b0 )( (b,a) => () => f(b(),a()) )
    new SeqF[B](l2)
  }
  def values() = l.map(_())
  //def futs() : Seq[Future[A]] = l.tail.scanLeft( future{l.head.apply()} )  ( (f:Future[A],x:()=>A) =>  f.map( _ => x() ))
}

class ViewImprovements[A](v :SeqView[A,Iterable[_]]) extends Logging {
  def scanLeftLazy[B](b0 : B)(f : (B,A)=>B) : SeqView[B,Seq[_]] = {
    // not doing this with real iterators
    class IterLite() {
      var b = b0
      var yet = false
      val it = v.iterator
      def next = if(yet) {
        logger.debug("ho")
        b = f(b,it.next)
        b
      } else {
        logger.debug("hum")
        yet = true
        b
      }
    }
    val iterator = new IterLite()
    v.map(ignored => iterator.next)
  }
}

object SeqF {
  class SeqFMaker[A](l : Seq[A]) {
    val it = l.iterator
    // val lf = for(i<-0 until l.length) yield () => it.next()
    //def wrapf() = new SeqF[A](lf)
    // must extract from l lazily!!!
    // def wrapf() = new SeqF[A](l.map x => () => x})
    def wrapf() = new SeqF(l.map( x => () => x) )
  }
  implicit def Seq2SeqFMakera[A](l : Seq[A]) = new SeqFMaker[A](l)

  implicit def SV2VI[A](l : SeqView[A,Iterable[_]]) = new ViewImprovements[A](l)

  trait FuturesMaker {
    def toParallelFutures()
  }

  def viewToParallelFutures[A](v:SeqView[A,Seq[A]]) : Seq[Future[A]] =  {
    val it=v.iterator
    val functors = for(i<-1 to v.length) yield future{it.next()}
    functors
  }

  // Is there a way of going from an IterableView to the iterator of futures
  // without knowing length?

  def viewToSequentialFutures[A](v:SeqView[A,Seq[A] forSome {type A}]) : Seq[Future[A]] =  {
    val it=v.iterator
    var f : Future[A] = null
    for(i<-1 to v.length) yield {
      f = if(f==null) future{it.next} else f.map((Unit)=>it.next)
      f
    }
  }

}









