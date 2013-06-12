  package com.podsnap.flip
  import java.io._
  import javax.swing.ImageIcon
  import java.awt.image.BufferedImage
  import scala.concurrent._
  import com.typesafe.scalalogging.slf4j.Logging
  import com.podsnap.flip.SeqF._
  import scala.util.Success
  import java.awt.Image

  //import org.apache.pdfbox.pdmodel.PDDocument
  //import org.apache.pdfbox.pdmodel.PDPage

  //import org.apache.pdfbox.util.PDFImageWriter
  //import org.apache.pdfbox.pdmodel


  class Smoother(pdf : String, progress : Int => Unit = null) extends Logging {

    val pdfdoc = new PDFDocument(pdf)
    val (h,w) = (pdfdoc.h,pdfdoc.w)
    val navg = 8
    val npix = h*w

    val pagesView = pdfdoc.view.
      map(imageToPixels).
      map(unpackIntsToLongs).
      scanLeftLazy(new Rolling(navg,npix))((r,ll)=>r.push(ll)).
      map(r => r.avg).
      map(packLongsToInts).
      map(pixelsToBufferedImage).
      zipWithIndex.
      map( p => new Page(p._1,p._2))

    logger.debug("Finished creating pagesView")
    val pages : Seq[Future[Page]] = viewToSequentialFutures(pagesView)

    def origPage(i : Int) : ImageIcon = {
      pdfdoc.getPageAsIcon(i)
    }

    def blurredPage(res : Int, p : Int) : ImageIcon = {
      // pages contains rolling averages, so the average from i to i+7 lives in i+7;  accordingly, get 4 ago
      if(p<navg) {
        origPage(p)
      } else
        pages(p).value match {
        case Some(Success(x:Page)) => x.icon
        case _ => origPage(p)
      }
    }

    def numPages = {
      pdfdoc.numPages
    }

    val tmpdir = File.createTempFile("pagesmoother","")
    tmpdir.delete()
    tmpdir.mkdir()
    sys.addShutdownHook {
      tmpdir.listFiles().map(_.delete())
      tmpdir.delete()
    }
    var files = new Array[File](numPages)
    private def imageFile(res : Int, p:Int) : File = {
      if(files(p)==null) {
        files(p) = new File(tmpdir.getAbsolutePath,"page_" + res + "_" + p + ".png")
      }
      files(p)
    }

    // print out 100 entries from the middle
    private def mondo[T](s : Seq[T]) = s.slice(s.length/2-50,s.length/2+50).mkString(",")

    // def mondo(a : Array) { a.mkString(",") }  // implicit only works in repl
    private def mondo[T](s : Array[T]) =  s.slice(s.length/2-50,s.length/2+50).mkString(",")

    private def unpack(packed : Int) : Long = {
      var p = packed
      var r = 0L
      r |= p & 0xff; r<<=16; p>>=8
      r |= p & 0xff; r<<=16; p>>=8
      r |= p & 0xff
      // complement, so black gets averaged to black
      ~r
    }
    private def unpackIntsToLongs(packed : Array[Int]) : Array[Long] = {
      logger.debug(s"entering unpackIntsToLongs")
      val p = packed.map(unpack)
      logger.trace(s"unpacked=${mondo{packed}}")
      p
    }

    private def pack(unpacked : Long) : Int = {
      var q = 0L
      var r = ~unpacked
      q |= r & 0xff; q<<=8; r>>=16
      q |= r & 0xff; q<<=8; r>>=16
      q |= r & 0xff
      q.toInt
    }
    private def packLongsToInts(unpacked : Array[Long]) : Array[Int] = {
      logger.debug("entering packLongsToInts")
      logger.trace(s"unpacking ${mondo(unpacked)}")
      unpacked.map(pack)
    }

    private def imageToPixels(img : Image) : Array[Int] = {
      val image = new ImageIcon(img).getImage
      val bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
      val g = bimage.createGraphics()
      g.drawImage(image,0,0,null)
      g.dispose()
      val pi = new Array[Int](npix)
      bimage.getRGB(0, 0, w, h, pi, 0, w)
      logger.debug(s"imageToPixels returning ${pi.toString}")
      logger.trace(s"imageToPixels returning ${mondo(pi)}")
      pi
    }
    private def pixelsToBufferedImage(pi : Array[Int]) : BufferedImage = {
      val bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
      bimage.setRGB(0, 0, w, h, pi, 0, w)
      logger.debug(s"pixelsToBufferedImage returning $bimage")
      bimage
    }

    class Page(bimage : BufferedImage , i : Int) {
      logger.debug(s"constructing Page($bimage ,$i)")
      javax.imageio.ImageIO.write( bimage,"png",imageFile(0,i))
      def icon = new ImageIcon(imageFile(0, i).getAbsolutePath)
      if(progress!=null) progress(i+1)
    }

    class PageFuture(fp: Future[Page], i: Int) {
      logger.debug(s"constructing PageFuture($fp,$i)")
      def icon = fp.value match {
        case Some(Success(x:Page)) => x.icon
        case _ => pdfdoc.getPageAsIcon(i)
      }
    }

  }

