  package com.podsnap.flip
  import java.io._
  import java.nio.channels.FileChannel
  import javax.swing.ImageIcon
  import java.awt.{Image, Rectangle}
  import com.sun.pdfview.PDFFile
  import java.awt.image.BufferedImage
  import java.util.concurrent.atomic.AtomicInteger
  import scala.concurrent._
  import com.typesafe.scalalogging.slf4j.Logging
  import ExecutionContext.Implicits.global
  import com.podsnap.flip.SeqF._
  import scala.util.Success

  //import org.apache.pdfbox.pdmodel.PDDocument
  //import org.apache.pdfbox.pdmodel.PDPage

  //import org.apache.pdfbox.util.PDFImageWriter
  //import org.apache.pdfbox.pdmodel


  class Smoother(pdf : String, progress : Int => Unit = null) extends Logging {

    val file = new File(pdf)
    val raf = new RandomAccessFile(file,"r")
    val channel = raf.getChannel
    val buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
    val pdffile = new PDFFile(buf)
    raf.close()

    val tmpdir = File.createTempFile("pagesmoother","")
    tmpdir.delete()
    tmpdir.mkdir()
    sys.addShutdownHook {
      tmpdir.listFiles().map(_.delete())
      tmpdir.delete()
    }


    val navg=8
    val bookend = 8

    val page = pdffile.getPage(0)
    val w = page.getBBox.getWidth.toInt
    val h = page.getBBox.getHeight.toInt

  //  val pdi = getPDImage(0)
  //  val w = pdi.getWidth()
  //  val h = pdi.getHeight()

    val rect = new Rectangle(0,0,w,h)

    val files = new Array[File](numPages)
    val done = new AtomicInteger(0)

    val pdfdoc = new PDFDocument(pdf)

//    var j=0
//    val v1 = pdfdoc.view
//    j = j+1; logger.debug(s"Created v$j")
//    val v2 = v1.map(imageToPixels)
//    j = j+1; logger.debug(s"Created v$j")
//    val v3 = v2.map(unpack)
//    j = j+1; logger.debug(s"Created v$j")
//    val v4 = v3.scanLeftThatWorks(new Rolling(8,w*h))((r,ll)=>r.push(ll))
//    j = j+1; logger.debug(s"Created v$j")
//    val v5 = v4.map(r=>r.avg)
//    j = j+1; logger.debug(s"Created v$j")
//    val v6 = v5.map(pack)
//    j = j+1; logger.debug(s"Created v$j")
//    val v7 = v6.map(pixelsToBufferedImage)
//    j = j+1; logger.debug(s"Created v$j")
//    val v8 = v7.zipWithIndex
//    j = j+1; logger.debug(s"Created v$j")
//    val v9 = v8.map(p => new Page(p._1,p._2))
//    j = j+1; logger.debug(s"Created v$j")
//
//    val pagesView = v9

    val pagesView = pdfdoc.view.
      map(imageToPixels).
      map(unpack).
      scanLeftThatWorks(new Rolling(8,w*h))((r,ll)=>r.push(ll)).
      map(r => r.avg).
      map(pack).
      map(pixelsToBufferedImage).
      zipWithIndex.
      map( p => new Page(p._1,p._2))

    logger.debug("Finished creating pagesView")
    val pages : Seq[Future[Page]] = viewToSequentialFutures(pagesView)

    // Possibly an abuse of future to launch an unsupervised thread
    // future {
    //  write()
    // }

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
      // pages(p).icon
      //      if(p<=bookend || p >=(numPages-bookend) || p>done.get())
      //        origPage(p)
      //      else
      //        new ImageIcon(imageFile(0, p).getAbsolutePath)
    }

    //  def getPDPage(i : Int) = cat.get(i) match {case p : PDPage => p}
  //  def getPDImage(i : Int) = getPDPage(i).convertToImage(BufferedImage.TYPE_3BYTE_BGR,100)

    def numPages = {
      pdffile.getNumPages
      // document.getNumberOfPages
    }

    private def imageFile(res : Int, p:Int) : File = {
      if(files(p)==null) {
        files(p) = new File(tmpdir.getAbsolutePath,"page_" + res + "_" + p + ".png")
      }
      files(p)
    }

    // print out 100 entries from the middle
    def mondo[T](s : Seq[T]) = s.slice(s.length/2-50,s.length/2+50).mkString(",")

    // def mondo(a : Array) { a.mkString(",") }  // implicit only works in repl
    def mondo[T](s : Array[T]) =  s.slice(s.length/2-50,s.length/2+50).mkString(",")

    def unpack(packed : Int) : Long = {
      var p = packed
      var r = 0L
      r |= (p&0xff); r<<=16; p>>=8
      r |= (p&0xff); r<<=16; p>>=8
      r |= (p&0xff)
      // complement, so black gets averaged to black
      ~r
    }
    def unpack(packed : Array[Int]) : Array[Long] = {
      logger.debug(s"entering unpack")
      val p = packed.map(unpack)
      logger.trace(s"unpacked=${mondo{packed}}")
      p
    }

    def pack(unpacked : Long) : Int = {
      var q = 0L
      var r = ~unpacked
      q |= (r&0xff); q<<=8; r>>=16
      q |= (r&0xff); q<<=8; r>>=16
      q |= (r&0xff)
      q.toInt
    }
    def pack(unpacked : Array[Long]) : Array[Int] = {
      logger.debug("entering pack")
      logger.trace(s"unpacking ${mondo(unpacked)}")
      unpacked.map(pack)
    }

    private def imageToPixels(img : Image) : Array[Int] = {
      val image = new ImageIcon(img).getImage
      val bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
      val g = bimage.createGraphics()
      g.drawImage(image,0,0,null)
      g.dispose()
      val pi = new Array[Int](h*w)
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

    class PageF(fp: Future[Page], i: Int) {
      logger.debug(s"constructing PageF($fp,$i)")
      def icon = fp.value match {
        case Some(Success(x:Page)) => x.icon
        case _ => pdfdoc.getPageAsIcon(i)
      }
    }

//    def averageInPage(i : Int, pixelsi : Array[Int], hist : Array[Array[Long]], rollingSum : Array[Long]) {
//      val im = i%navg
//      for(j <- 0 until h*w) {
//        var p = pixelsi(j)
//        val r = unpack(p)
//        val leaver = hist(im)(j)
//        hist(im)(j) = r
//        rollingSum(j) = rollingSum(j) + r - leaver
//        pixelsi(j) =  if (i>=navg)  pack((rollingSum(j)*3L/navg + r)/4) else p
//      }
//    }
//
//
//    def write() {
//
//      val numPgs = numPages
//
//      // Unfunctional scratch pads
//      val rollingSum = Array.fill[Long](h*w)(0)
//      val hist = Array.ofDim[Long](numPgs,h*w)
//      var bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
//
//      def writePage(i : Int, pi : Array[Int]) {
//        try {
//          bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
//          bimage.setRGB(0, 0, w, h, pi, 0, w)
//        } catch {
//          case e : Throwable => logger.error(s"horrible error on page $i  $e")
//        }
//
//        //save it as a file
//        if(i>=navg) {
//          try {
//            val ii = i-navg/2 // middle of averaging range
//            javax.imageio.ImageIO.write( bimage,"png",imageFile(0,ii+1))
//          }
//          catch {
//            case e : Throwable =>
//              throw new RuntimeException(e.getMessage)
//          }
//        }
//      }
//
//      var i = 0
//
//
//      //for(i<- (0 until numPgs).view ) {
//      for(pixelsi <- pdfdoc.iteratorOfNewIntArray()) {
//
//        logger.debug(s"Processing page $i of $numPgs")
//        // val pixelsi = pdfdoc.getPageAsCachedIntArray(i);
//
//        averageInPage(i,pixelsi,hist,rollingSum)
//
//        writePage(i,pixelsi)
//
//        val ii = i-navg/2 // middle of averaging range
//        if(progress!=null) progress(ii+1)
//        done.set(ii)
//        i = i + 1
//      }
//    }

  }

