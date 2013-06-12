package com.podsnap.flip
import com.sun.pdfview.{PDFPage, PDFFile}
import java.awt.image.BufferedImage
import java.awt.Image
import java.io._
import java.nio.channels.FileChannel
import javax.swing.ImageIcon
import java.awt.{Image, Rectangle}
import com.typesafe.scalalogging.slf4j.Logging

class PDFDocument(pdf : String) extends Iterable[Image] with Seq[Image] with Logging{

  private val pdffile : PDFFile= {
    val file = new File(pdf)
    val raf = new RandomAccessFile(file,"r")
    val channel = raf.getChannel
    val buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
    val p = new PDFFile(buf)
    raf.close()
    p
  }
  private val h = pdffile.getPage(0).getBBox.getHeight.toInt
  private val w = pdffile.getPage(0).getBBox.getWidth.toInt
  private val rect = new Rectangle(0,0,w,h)

  val numPages = pdffile.getNumPages()
  def length = numPages

  def getPageAsIcon(i : Int) = {
    val page = pdffile.getPage(i-1)
    val ic = new ImageIcon(page.getImage(w,h,rect,null,true,true))
    ic
  }

  def getPageAsImage(i : Int) = {
    logger.debug(s"getPageAsImage($i)")
    this.synchronized {  // to be safe
      pdffile.getPage(i-1).getImage(w,h,rect,null,true,true)
    }
  }
  class IteratorOfImage() extends Iterator[Image] {
    var i = 0
    def next() = {val ii=i; i=i+1; getPageAsImage(i)}
    def hasNext = i<numPages
  }
  def iterator() = new IteratorOfImage()


  private def getPageIntArray(i : Int, pi:Array[Int], bimage : BufferedImage) : Array[Int] = {
    var image = pdffile.getPage(i).getImage(w,h,rect,null,true,true)
    // var image : Image = getPDImage(i)
    image = new ImageIcon(image).getImage
    val g = bimage.createGraphics()
    // Paint the image onto the buffered image
    g.drawImage(image, 0, 0, null)
    g.dispose()
    bimage.getRGB(0, 0, w, h, pi, 0, w)
    pi
  }
  val pixelsi = new Array[Int](h*w)
  val bimage =  new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
  def getPageAsCachedIntArray(i : Int) = getPageIntArray(i,pixelsi,bimage)
  def getPageAsNewIntArray(i : Int) =
      {getPageIntArray(i,new Array[Int](h*w), new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR))}

  class IteratorOfIntArray(f: Int=> Array[Int]) extends Iterator[Array[Int]]{
    var i = 0
    def next() = {val ia = f(i); i=i+1; ia}
    def hasNext() = i<numPages
  }


  def iteratorOfNewIntArray() : Iterator[Array[Int]] = new IteratorOfIntArray(getPageAsNewIntArray)
  def iteratorOfCachedIntArray() : Iterator[Array[Int]] = new IteratorOfIntArray(getPageAsCachedIntArray)

  //def iterator() = iteratorOfNewIntArray()

  def apply(i : Int) = getPageAsImage(i)

}
