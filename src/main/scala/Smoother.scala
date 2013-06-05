package com.podsnap.flip
import java.io._
import java.nio.channels.FileChannel
import javax.swing.ImageIcon
import java.awt.Rectangle
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import com.sun.pdfview.PDFFile  // Why does it claim this is an error when sbt doesn't think it is?

// TODO: figure out how to run something in the background and listen for it being done

class Smoother(pdf : String, progress : Int => Unit = null) {

  val file = new File(pdf)
  val raf = new RandomAccessFile(file,"r")
  val channel = raf.getChannel()
  val buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
  val pdffile = new PDFFile(buf)
  raf.close()
  val tmpdir = File.createTempFile("pagesmoother","")
  tmpdir.delete()
  tmpdir.mkdir()

  val page = pdffile.getPage(0)
  val w : Int = page.getBBox().getWidth().toInt
  val h : Int = page.getBBox().getHeight().toInt
  val rect = new Rectangle(0,0,w,h)

  future {
    write()
  }

  def getPage(i : Int) {
    pdffile.getPage(i)
  }

  def numPages = {
    pdffile.getNumPages()
  }

  def origPage(i : Int) : ImageIcon = {
    val page = pdffile.getPage(i-1)
    val ic = new ImageIcon(page.getImage(w,h,rect,null,true,true))
    ic
  }

  def write() {
    val numPgs = pdffile.getNumPages();
    for(i <- 1 to numPgs) {
      Thread.sleep(500)
      if(progress!=null) progress(i)  // this feels wrong
    }
  }

}













