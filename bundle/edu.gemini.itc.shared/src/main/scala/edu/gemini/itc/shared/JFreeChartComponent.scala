package edu.gemini.itc.shared

import java.awt.Color

import org.jfree.chart.{ChartPanel, JFreeChart}

import scala.swing.Component

// a very simple Scala wrapper for JFreeChart charts
class JFreeChartComponent(chart: JFreeChart) extends Component {
  override lazy val peer = new ChartPanel(chart)
  peer.setMaximumDrawHeight(Int.MaxValue)                       // don't limit drawing resolution
  peer.setMaximumDrawWidth(Int.MaxValue)                        // don't limit drawing resolution
  peer.setBackground(Color.white)
}
