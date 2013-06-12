package com.podsnap.flip
import com.typesafe.scalalogging.slf4j.Logging
import scala.concurrent._
import ExecutionContext.Implicits.global

//import java.awt.FlowLayout
import scala.swing._
//import javax.swing._

// The best way to make something happen on the UI thread is to publish an event, which requires
// being in a class that extends Publisher.
private case class StatusEvent(i : Int) extends event.Event {}
private case class StoppedFlippingEvent() extends event.Event {}

object Flippotron  extends App with Logging {

  var file : String = _
  override def main(args : Array[String]) {
    file = if(args.length>0) args(0) else "/home/pnf/Downloads/scala-swing-design"
    GUI.main(args)
  }


  object GUI extends SimpleSwingApplication with Publisher with Logging {

    val positionField = new TextField("1",5) {editable = false}
    val statusField = new TextField("",5) {editable = false}
    val flip = new Label()
    val smoother = new Smoother(file, { i : Int => publish(new StatusEvent(i))})
    val numPages = {
      smoother.numPages
    }
    val pageSlider = new Slider {
      min = 1
      max = numPages
      preferredSize = new Dimension(750,20)
    }
    val controlPanel = new BoxPanel(Orientation.Horizontal) {
      contents += positionField
      contents += pageSlider
      contents += statusField
    }
    val flipPanel = new FlowPanel() {
      contents += flip
    }

    val rest = new java.util.concurrent.atomic.AtomicLong()
    val timer : Future[Unit] = future {
      // send a stopped flipping event in 10 seconds
      rest.set(now)   // time of last motion
      while (true){
        val dt = scala.compat.Platform.currentTime - rest.get()
        if(dt>=1000) {                 scala.compat.Platform.currentTime
          publish(new StoppedFlippingEvent())
          Thread.sleep(1000)
        } else {
          Thread.sleep(1000-dt)
        }
      }
    }

    def now : Long = {
      scala.compat.Platform.currentTime
    }

    def top = new MainFrame {
      title = "Flippotron"
      contents = new BoxPanel(Orientation.Vertical) {
        contents += controlPanel
        contents += flipPanel
      }
      listenTo(pageSlider, GUI)
      var lastp = 1
      var lastnow = now
      reactions += {
        case swing.event.ValueChanged(`pageSlider`) =>
          val i = pageSlider.value
          val newnow = now
          val dt = newnow - lastnow
          logger.debug(s"received slider event $i $dt")
          positionField.text = i+""
          if(dt > 100) {
            flip.icon = smoother.origPage(i)
          } else {
            flip.icon = smoother.blurredPage(0,i)
            rest.set(now)
          }
          lastnow = newnow
          lastp = i
          pack()
        case StatusEvent(i) =>
          logger.debug(s"received status event $i")
          statusField.text = numPages + "(" + i + ")"
          if(i<=1) {
            flip.icon = smoother.origPage(i)
            pack()
          }
        case StoppedFlippingEvent() =>
          logger.debug(s"stopped flipping event")
          flip.icon = smoother.origPage(lastp)

      }
    }
  }
}
