package com.podsnap.flip
import com.typesafe.scalalogging.slf4j.Logging


//import java.awt.FlowLayout
import scala.swing._
//import javax.swing._

private case class StatusEvent(i : Int) extends event.Event {}

object Flippotron  extends SimpleSwingApplication with Publisher with Logging {
  val file = "bleh.pdf"
  val positionField = new TextField("1",5) {editable = false}
  val statusField = new TextField("",5) {editable = false}
  val flip = new Label()
  val smoother = new Smoother("/home/pnf/Downloads/scala-swing-design.pdf", { i : Int => publish(new StatusEvent(i))})
  val numPages = smoother.numPages
  val pageSlider = new Slider {
    min = 1
    max = numPages
    preferredSize = new Dimension(500,20)
  }
  val controlPanel = new BoxPanel(Orientation.Horizontal) {
    contents += positionField
    contents += pageSlider
    contents += statusField
  }
  val flipPanel = new FlowPanel() {
    contents += flip
  }

  def top = new MainFrame {
    title = "Flippotron"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += controlPanel
      contents += flipPanel
    }
    listenTo(pageSlider, Flippotron)
    reactions += {
      case swing.event.ValueChanged(`pageSlider`) =>
        val i = pageSlider.value
        positionField.text = i+""
        flip.icon = smoother.origPage(i)
        pack
      case StatusEvent(i) =>
        logger.debug(s"received status event $i")
        statusField.text = numPages + "(" + i + ")"
        if(i<=1) {
          flip.icon = smoother.origPage(i)
          pack()
        }
    }
  }
}
