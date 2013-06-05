package com.podsnap.flip
import java.io._
import java.nio.channels.FileChannel
import javax.swing.ImageIcon
import java.awt.{Graphics, Rectangle}
import com.sun.pdfview.PDFFile
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent._
import ExecutionContext.Implicits.global

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

  val navg=8
  val bookend = 8

  val page = pdffile.getPage(0)
  val wr : Int = page.getBBox().getWidth().toInt
  val hr : Int = page.getBBox().getHeight().toInt
  val rect = new Rectangle(0,0,wr,hr)
  val w = wr
  val h = hr


  val files = new Array[File](numPages)

  val done = new AtomicInteger(0)

  // Possibly an abuse of future to launch an unsupervised thread
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

  def blurredPage(res : Int, p : Int) = {
    if(p<=bookend || p >=(numPages-bookend) || p>done.get())
      origPage(p);
    else
      new ImageIcon(imageFile(0,p).getAbsolutePath())
  }

  private def imageFile(res : Int, p:Int) : File = {
    if(files(p)==null)
    files(p) = new File(tmpdir.getAbsolutePath(),"page_" + res + "_" + p + ".png");
    files(p)
  }

  def write() {
    val numPgs = pdffile.getNumPages()
    val pixelsi = new Array[Int](h*w)
    val rollingSum = Array.fill[Long](h*w)(0)
    val hist = Array.ofDim[Long](numPgs,h*w)
    var bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)

      //		BufferedImage simage = null;
      //	    float data[] = { 0.0625f, 0.125f, 0.0625f, 0.125f, 0.25f, 0.125f,
      //	            0.0625f, 0.125f, 0.0625f };
      //	    Kernel kernel = new Kernel(3, 3, data);
      //	    ConvolveOp convolve = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null);

    for(i<-0 until numPgs) {

        //generate page image
        var image = pdffile.getPage(i).getImage(w,h,rect,null,true,true)
        // force complete loading
        image = new ImageIcon(image).getImage()
        // Copy image to buffered image
        val g = bimage.createGraphics()
        // Paint the image onto the buffered image
        g.drawImage(image, 0, 0, null)
        g.dispose()

        // extract pixels into array
        bimage.getRGB(0, 0, w, h, pixelsi, 0, w)
        // Accumulate rolling averages
        val im = i%navg
        val ii = i-navg/2; // middle of averaging range
        for(j <- 0 until h*w) {
          var p = pixelsi(j)
          var q=0L

          // Expand packed 8x3 pixel to 16x3
          var r = 0L
          var r2 = 0L
          r |= (p&0xff); r<<=16; p>>=8;
          r |= (p&0xff); r<<=16; p>>=8;
          r |= (p&0xff);
          // complement, so black gets averaged to black
          r = ~r
          rollingSum(j) = rollingSum(j) + r

          if(i>=navg) { // we have enough to average
            rollingSum(j) = rollingSum(j) - hist(im)(j);    // roll off the old
            hist(im)(j) = r

            rollingSum(j)*3

            r2 = (rollingSum(j)*3L/navg + r)/4;
            r = ~r2;
          } else {
            hist(im)(j) = r;
          }

          // Repack averaged pixel
          q |= (r&0xff); q<<=8; r>>=16;
          q |= (r&0xff); q<<=8; r>>=16;
          q |= (r&0xff);

        //	Average over number of images frames with a non-background pixel in this location.
        //  Note that we sum in complement space, so background is zero.
        //				if(i>=navg)
        //				nfg[j] -= fghist[im][j];
        //			nfg[j] += (fghist[im][j] = (q==-1) ? 0 : 1);
        //
        //				// If all pixels in history were background, this is easy...
        //				if(nfg[j]==0)
        //					q = -1;
        //
        //				else {
        //
        //					if(i>=navg) sum[k]-=hist[im][k];
        //					hist[im][k] = ~(p&0xff);
        //					sum[k] += hist[im][k];
        //					if(i>=navg)
        //						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
        //					else if(i>=navg/2)
        //						q += ~(sum[k]/nfg[j] + hist[i-navg/2][k])>>1;
        //					else
        //						q += ~(sum[k]/nfg[j]);
        //					k++; q<<=8; p>>=8;
        //
        //					if(i>=navg) sum[k]-=hist[im][k];
        //					hist[im][k] = ~(p&0xff);
        //					sum[k] += hist[im][k];
        //					if(i>=navg)
        //						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
        //				else if(i>=navg/2)
        //				q += ~((sum[k]/nfg[j] + hist[i-navg/2][k])>>1);
        //			else
        //				q += ~(sum[k]/nfg[j]);
        //			k++; q<<=8; p>>=8;
        //
        //					if(i>=navg) sum[k]-=hist[im][k];
        //					hist[im][k] = ~(p&0xff);
        //					sum[k] += hist[im][k];
        //					if(i>=navg)
        //						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
        //					else if(i>=navg/2)
        //						q += ~((sum[k]/nfg[j] + hist[i-navg/2][k])>>1);
        //					else
        //						q += ~(sum[k]/nfg[j]);
        //					k++;
        //				}

        pixelsi(j) = q.toInt;
      }
      bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR);
      bimage.setRGB(0, 0, w, h, pixelsi, 0, w);

      //save it as a file
      if(i>=navg) {
        try {
          javax.imageio.ImageIO.write( bimage,"png",imageFile(0,ii+1))
        }
        catch {
          case e : Throwable =>
            throw new RuntimeException(e.getMessage());
        }
      }
      if(progress!=null) progress(ii+1)

      done.set(ii)

      // todo: handle termination

    }
  }

}











