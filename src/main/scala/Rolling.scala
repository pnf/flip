package com.podsnap.flip

import com.typesafe.scalalogging.slf4j.Logging


// Holder intermediates of a rolling average.
class Rolling(window: Int, length : Int) extends Logging {

  logger.debug(s"Creating new Rolling $window $length")

  var i = 0
  val hist = Array.ofDim[Long](window,length)
  val sum = new Array[Long](length)
  val buf = new Array[Long](length)

  def mondo[T](s : Array[T]) =  s.slice(s.length/2-50,s.length/2+50).mkString(",")

  def push(v : Array[Long]) : Rolling = {
    logger.trace(s"pushing ${mondo(v)}")
    val im = i%window
    i = i + 1
    var j=0
    var tmp = 0L
    // This is apparently fast
    while(j<length) {
      sum (j) = sum(j) - hist(im)(j) + v(j)
      hist(im)(j) = v(j)
      j = j+1
    }
    this
  }

  def avg : Array[Long] = {
    val im = i%window
    val a = if(i<window)
      hist(im)
    else {
      var j = 0
      while(j<length) {
        //buf(j) = (3*sum(j)/length + hist(im)(j))/4
        buf(j) = sum(j)/window
        j = j+1
      }
      buf
    }
    logger.trace(s"avg=${mondo(a)}")
    a
  }

}
