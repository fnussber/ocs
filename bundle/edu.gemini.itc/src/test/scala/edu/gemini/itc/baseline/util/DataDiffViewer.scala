package edu.gemini.itc.baseline.util

import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JSpinner, SpinnerNumberModel}

import edu.gemini.itc.baseline._
import edu.gemini.itc.shared._

import scala.annotation.tailrec
import scala.swing.GridBagPanel.Anchor
import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}


final class DataDiffViewer[F <: InstrumentDetails](fixtures: List[Fixture[F]]) extends MainFrame {

  val prev    = new Button("<<<<")
  val next    = new Button(">>>>")

  val group   = new Component {
    override lazy val peer = new JSpinner(new SpinnerNumberModel(0, 0, 20, 1))
    def value = Integer.parseInt(peer.getValue.toString)
    peer.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = publish(SelectionChanged(null)) // TODO
    })
  }
  val chartT  = new ComboBox[SpcChartType](SpcChartType.Values)
  val dataT   = new ComboBox[SpcDataType](SpcDataType.Values)

  def curGroup   = group.value
  def curChart   = chartT.selection.item
  def curData    = dataT.selection.item


  val info    = new InfoPanel
  val panel   = new DataDiffPanel(info)

  title       = "ITC Data Diff Viewer"
  visible     = true
  contents    = panel
  preferredSize = new Dimension(1200,800)
  pack()

  // == view first
  // TODO: make sure baseline is properly initialised
  var curFixture = 0
  var curIndex   = Index(fixtures.head, Baseline.cook(fixtures.head).get) // TODO: how to deal with get() on cook()
  view(fixtures(curFixture))

  listenTo(prev, next, chartT.selection, dataT.selection, group)
  reactions += {
    case ButtonClicked(`prev`) =>
      curFixture = Math.max(0, curFixture - 1)
      view(fixtures(curFixture))

    case ButtonClicked(`next`) =>
      curFixture = Math.min(fixtures.length-1, curFixture + 1)
      nextDiff(fixtures, curIndex) match {
        case Some(i)  => view(i)
        case None     => view(fixtures.head) // TODO: what to do if we don't have any differences??
      }


    case SelectionChanged(_) =>
      view(fixtures(curFixture))
  }

  private def seriesData(r: ItcSpectroscopyResult): Option[List[SpcSeriesData]] =
    r.series(curChart, curData, curGroup)

  private def seriesData(r: ItcSpectroscopyResult, g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx): Option[SpcSeriesData] =
    r.series(c, d, g).map(_(s))

  // ======== CHECK FOR DIFF
  // Checks if the current data matches the baseline. Will return None if baseline size does not match, true/false else.
  private def isDiff(cur: Index[F]): Option[Boolean] = for {
      d <- cur.data
      b <- cur.baseline
      if d.length == b.length
      d2 = d.map(_.compress)
    } yield d2.zip(b).forall { case (d0,d1) => d0 ~= d1 } // TODO define ~= specific to this use case?

  // ======== FIND NEXT DIFF
  @tailrec private def nextDiff(fs: List[Fixture[F]], cur: Index[F]): Option[Index[F]] = {

    // next fixture (if any)
    def nextF() = fs.indexOf(cur.f) + 1 match {
      case i if i == fs.length => None
      case i                   => Some(fs(i))
    }
    // next iter (if any), this will also skip any positions for which no data is available in the result
    def next(): Option[Index[F]] = cur.next() match {
      case None                 => nextF().map(f => Index(f, Baseline.cook(f).get)) // TODO: deal with get() on cook()
      case Some(n)              => Some(n) //if (n.data.isDefined) Some(n) else next()
    }
    // check for difference?
    def diff(i: Index[F]) = isDiff(i) match {
      case _ => true  // TODO: for testing the iteration just say it's always different...
// TODO: activate the following to lines to actually check for differences!
//      case None                 => false // TODO: what to do here: there seems to be a structural difference in the data, this is "ok" if we check for data that doesn't exist
//      case Some(b)              => b
    }

    // advance iterator forward to next difference
    next() match {
      case None                       =>
        None
      case Some(i) if i.data.isEmpty  =>
        nextDiff(fs, i)
      case Some(i)                    =>
        if (diff(i)) Some(i) else nextDiff(fs, i)
    }

  }

  private def view(i: Index[F]): Unit = {
    info.show(i)
    curIndex = i
    val dataSeries: List[SpcSeriesData]     = seriesData(i.r).getOrElse(List())
    val baselineSeries: List[SpcSeriesData] = seriesData(i.r).map { _.zipWithIndex.map { case (s, ix) =>
      // create series based on existing one with new data
      val bline = Baseline.baselineData(i.f, curGroup, curChart, curData, ix).get // TODO: deal with get
      s.copy(title = s"Baseline: ${s.title}", data = bline.toArrays, color = Some(ITCChart.DarkRed))
    }}.getOrElse(List())


    // ====
    val series2 = dataSeries ++ baselineSeries
    // TODO: get rid of get here, above we already know if we have data or not
    val ch0 = i.r.chart(curChart, curGroup).getOrElse(SpcChartData(curChart, "NO DATA", ChartAxis("x"), ChartAxis("y"), List()))
    val ch2: SpcChartData = ch0.copy(series = series2)

    val c = ITCChart.forSpcDataSet(ch2, PlottingDetails.Auto)
    panel.showChart(new JFreeChartComponent(c.getChart))

  }


  private def view(f: Fixture[F]): Unit = {
    info.show(f)
    Baseline.cook(f).foreach { r =>

      val dataSeries: List[SpcSeriesData]     = seriesData(r).getOrElse(List())
      val baselineSeries: List[SpcSeriesData] = seriesData(r).map { _.zipWithIndex.map { case (s, ix) =>
        // create series based on existing one with new data
        val bline = Baseline.baselineData(f, curGroup, curChart, curData, ix).get // TODO: deal with get
        s.copy(title = s"Baseline: ${s.title}", data = bline.toArrays, color = Some(ITCChart.DarkRed))
      }}.getOrElse(List())


      // ====
      val series2 = dataSeries ++ baselineSeries
      // TODO: get rid of get here, above we already know if we have data or not
      val ch0 = r.chart(curChart, curGroup).getOrElse(SpcChartData(curChart, "NO DATA", ChartAxis("x"), ChartAxis("y"), List()))
      val ch2: SpcChartData = ch0.copy(series = series2)

      val c = ITCChart.forSpcDataSet(ch2, PlottingDetails.Auto)
      panel.showChart(new JFreeChartComponent(c.getChart))

    }
  }

  final class InfoPanel extends GridBagPanel {

    import scala.swing.GridBagPanel.Anchor._
    import scala.swing.GridBagPanel.Fill._

    val fixLabel = new Label()
    val insLabel = new Label()
    val obsLabel = new Label()
    val conLabel = new Label()
    val srcLabel = new Label()
    val tepLabel = new Label()
    val chartPanel = new FlowPanel(new Label("Group: "), group, new Label("Chart: "), chartT, new Label("Data: "), dataT)

    layout(new Label("Fixture:"))     = new Constraints { gridx=0;gridy=0;anchor=West }
    layout(fixLabel)                  = new Constraints { gridx=1;gridy=0;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Chart:"))       = new Constraints { gridx=0;gridy=1;anchor=West }
    layout(chartPanel)                = new Constraints { gridx=1;gridy=1;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Instrument:"))  = new Constraints { gridx=0;gridy=2;anchor=West }
    layout(insLabel)                  = new Constraints { gridx=1;gridy=2;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Observation:")) = new Constraints { gridx=0;gridy=3;anchor=West }
    layout(obsLabel)                  = new Constraints { gridx=1;gridy=3;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Conditions:"))  = new Constraints { gridx=0;gridy=4;anchor=West }
    layout(conLabel)                  = new Constraints { gridx=1;gridy=4;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Source:"))      = new Constraints { gridx=0;gridy=5;anchor=West }
    layout(srcLabel)                  = new Constraints { gridx=1;gridy=5;weighty=1;fill=Horizontal;anchor=West }
    layout(new Label("Telescope:"))   = new Constraints { gridx=0;gridy=6;anchor=West }
    layout(tepLabel)                  = new Constraints { gridx=1;gridy=6;weighty=1;fill=Horizontal;anchor=West }

    def show(f: Fixture[F]) {
      fixLabel.text = s"ID = ${f.hash}   --   # ${curFixture+1} / ${fixtures.length}"
      insLabel.text = f.ins.toString
      obsLabel.text = f.odp.toString
      conLabel.text = f.ocp.toString
      srcLabel.text = f.src.toString
      tepLabel.text = f.tep.toString
    }

    def show(i: Index[F]) {
      deafTo(chartT.selection, dataT.selection, group)
      val f = i.f
      group.peer.setValue(i.g)
      chartT.selection.item = i.c
      dataT.selection.item = i.d
      fixLabel.text = s"ID = ${f.hash}   --   # ${fixtures.indexOf(f)+1} / ${fixtures.length}"
      insLabel.text = f.ins.toString
      obsLabel.text = f.odp.toString
      conLabel.text = f.ocp.toString
      srcLabel.text = f.src.toString
      tepLabel.text = f.tep.toString
      listenTo(chartT.selection, dataT.selection, group)
    }
  }

  final class DataDiffPanel(info: InfoPanel) extends GridBagPanel {

    import scala.swing.GridBagPanel.Fill._

    var chart: Component = new Label() // placeholder

    layout(prev)  = new Constraints { gridx = 0; weighty = 1; gridheight = 2; fill = Vertical}
    layout(info)  = new Constraints { gridx = 1; gridy = 0; anchor = Anchor.NorthWest}
    layout(chart) = new Constraints { gridx = 1; gridy = 1; weightx = 1; weighty = 1; fill = Both}
    layout(next)  = new Constraints { gridx = 2; weighty = 1; gridheight = 2; fill = Vertical}

    def showChart(newChart: Component) = {
      layout.remove(chart)
      chart = newChart
      layout(newChart) = new Constraints {
        gridx = 1; weightx = 1; weighty = 1; fill = Both
      }
      revalidate()
    }

  }

}
