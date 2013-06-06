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
  //import org.apache.pdfbox.pdmodel.PDDocument
  //import org.apache.pdfbox.pdmodel.PDPage


  import org.apache.pdfbox.util.PDFImageWriter
  import org.apache.pdfbox.pdmodel
  ;

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

    //val document = PDDocument.load(file)
    //val cat = document.getDocumentCatalog.getAllPages()  // this is actually a list of Any

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

    // Possibly an abuse of future to launch an unsupervised thread
    future {
      write()
    }

  //  def getPDPage(i : Int) = cat.get(i) match {case p : PDPage => p}
  //  def getPDImage(i : Int) = getPDPage(i).convertToImage(BufferedImage.TYPE_3BYTE_BGR,100)

    sys.addShutdownHook {
      tmpdir.listFiles().map(_.delete())
      tmpdir.delete()
    }

    def numPages = {
      pdffile.getNumPages
      // document.getNumberOfPages
    }

    def origPage(i : Int) : ImageIcon = {
      val page = pdffile.getPage(i-1)
      val ic = new ImageIcon(page.getImage(w,h,rect,null,true,true))
      ic

      // new ImageIcon(getPDImage(i));
    }

    def blurredPage(res : Int, p : Int) = {
      if(p<=bookend || p >=(numPages-bookend) || p>done.get())
        origPage(p)
      else
        new ImageIcon(imageFile(0, p).getAbsolutePath)
    }

    private def imageFile(res : Int, p:Int) : File = {
      if(files(p)==null) {
        files(p) = new File(tmpdir.getAbsolutePath,"page_" + res + "_" + p + ".png")
      }
      files(p)
    }


    def unpack(packed : Int) : Long = {
      var p = packed
      var r = 0L
      r |= (p&0xff); r<<=16; p>>=8
      r |= (p&0xff); r<<=16; p>>=8
      r |= (p&0xff)
      // complement, so black gets averaged to black
      ~r
    }

    def pack(unpacked : Long) : Int = {
      var q = 0L
      var r = ~unpacked
      q |= (r&0xff); q<<=8; r>>=16
      q |= (r&0xff); q<<=8; r>>=16
      q |= (r&0xff)
      q.toInt
    }

    def pixelPage(i : Int, pixelsi : Array[Int], bimage : BufferedImage) {
      var image = pdffile.getPage(i).getImage(w,h,rect,null,true,true)
      // var image : Image = getPDImage(i)
      image = new ImageIcon(image).getImage
      // Copy image to buffered image
      val g = bimage.createGraphics()
      // Paint the image onto the buffered image
      g.drawImage(image, 0, 0, null)
      g.dispose()
      // extract pixels into array
      bimage.getRGB(0, 0, w, h, pixelsi, 0, w)
    }


    def averageInPage(i : Int, pixelsi : Array[Int], hist : Array[Array[Long]], rollingSum : Array[Long]) {
      val im = i%navg
      for(j <- 0 until h*w) {
        var p = pixelsi(j)
        val r = unpack(p)
        val leaver = hist(im)(j)
        hist(im)(j) = r
        rollingSum(j) = rollingSum(j) + r - leaver
        pixelsi(j) =  if (i>=navg)  pack((rollingSum(j)*3L/navg + r)/4) else p
      }
    }



    def write() {

      val numPgs = numPages

      // Unfunctional scratch pads
      val pixelsi = new Array[Int](h*w)
      val rollingSum = Array.fill[Long](h*w)(0)
      val hist = Array.ofDim[Long](numPgs,h*w)
      var bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)

      def writePage(i : Int) {
        try {
          bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
          bimage.setRGB(0, 0, w, h, pixelsi, 0, w)
        } catch {
          case e : Throwable => logger.error(s"horrible error on page $i  $e")
        }

        //save it as a file
        if(i>=navg) {
          try {
            val ii = i-navg/2 // middle of averaging range
            javax.imageio.ImageIO.write( bimage,"png",imageFile(0,ii+1))
          }
          catch {
            case e : Throwable =>
              throw new RuntimeException(e.getMessage)
          }
        }
      }

      for(i<- (0 until numPgs).view ) {

        logger.debug(s"Processing page $i of $numPgs")
        pixelPage(i,pixelsi,bimage)
        averageInPage(i,pixelsi,hist,rollingSum)

        writePage(i)

        val ii = i-navg/2 // middle of averaging range
        if(progress!=null) progress(ii+1)
        done.set(ii)

      }
    }

  }

