package edu.gemini.itc.baseline.util

import java.io._
import java.util.zip._
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{SpinnerNumberModel, JSpinner}

import edu.gemini.itc.acqcam.AcqCamRecipe
import edu.gemini.itc.base.{DatFile, SpectroscopyArrayRecipe, SpectroscopyRecipe, Recipe}
import edu.gemini.itc.baseline._
import edu.gemini.itc.flamingos2.Flamingos2Recipe
import edu.gemini.itc.gmos.GmosRecipe
import edu.gemini.itc.gnirs.GnirsRecipe
import edu.gemini.itc.gsaoi.GsaoiRecipe
import edu.gemini.itc.michelle.MichelleRecipe
import edu.gemini.itc.nifs.NifsRecipe
import edu.gemini.itc.niri.NiriRecipe
import edu.gemini.itc.shared._
import edu.gemini.itc.trecs.TRecsRecipe
import edu.gemini.spModel.core.AlmostEqual

import AlmostEqual.AlmostEqualOps

import scala.annotation.tailrec
import scala.swing.GridBagPanel.Anchor
import scala.swing._
import scala.swing.event.{SelectionChanged, ButtonClicked}

// A very bare bones data object for sampling stuff.
final case class Data(values: Array[(Double, Double)]) {

  def x(i: Int) = values(i)._1
  def y(i: Int) = values(i)._2

  lazy val xValues = values.map(_._1).toList
  lazy val yValues = values.map(_._2).toList

  def compress: Data =
    this.elimZeros.resample(2) // TODO: resample with 2nm, allow 1nm in case total amount of data is manageable

  // eliminate repeating zero values
  private def elimZeros: Data =
    copy(values = values.iterator.sliding(3,2).flatMap {
      case Seq((_,0),(_,0),(_,0)) => Seq()
      case Seq(a,b,_)             => Seq(a,b)
      case s                      => s
    }.toArray)

  // resample to 2nm (some data is sampled down to 0.2nm precision), TODO: pass desired decision instead of calculating the value?
  // TODO: use squants for wavelength values?? (note: some charts use pixels here..)
  private def resample(nm: Int): Data = {

    def combine = math.max(1,(nm/math.abs(x(0)-x(1))).toInt)

    def avg(s: Iterable[(Double, Double)]): Seq[(Double, Double)] =
      Seq(s.fold((0.0, 0.0))({ case (a, b) => (a._1 + b._1, a._2 + b._2) })).map { case (x, y) => (x / combine, y / combine) } // TODO: simplify

    // TODO: deal with case that eliminating zeroes entirely wiped out the data!
    if (values.length < 2) return this

    copy(values = values.sliding(combine, combine).flatMap {
      case s if s.exists { case (x, y) => y == 0 }  => s        // do NOT fiddle with 0 values!
      case s                                        => avg(s)
    }.toArray)

  }

  def toArrays: Array[Array[Double]] = {
    val a: Array[Array[Double]] = new Array(2)
    a(0) = xValues.toArray
    a(1) = yValues.toArray
    a
  }

  def ~=(other: Data): Boolean =
    (xValues ~= other.xValues) && (yValues ~= other.yValues)

}
object Data {
  def fromSeries(s: SpcSeriesData) = {
    require(s.xValues.length == s.yValues.length, "same amount of x and y values required")
    require(s.xValues.length >= 2)
    new Data(s.xValues.zip(s.yValues))
  }

  def apply(data: Array[Array[Double]]): Data = {
    require(data.length == 2)
    require(data(0).length == data(1).length)
    Data(data(0).zip(data(1)))
  }
}

final class DataDiffViewer[F <: InstrumentDetails](file: String, fixtures: List[Fixture[F]]) extends MainFrame {

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

  // == write/read baseline
//  write(file, fixtures)
  val baseline = read(file)

  // == view first
  // TODO: make sure baseline is properly initialised
  var curFixture = 0
  var curIndex   = Index(fixtures.head, cook(fixtures.head).get) // TODO: how to deal with get() on cook()
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


  // TODO: move this to baseline
  //======= WRITE BASELINE DATA
  def write(file: String, fs: List[Fixture[F]]): Unit = {
    val o = new FileOutputStream(file)
    val z = new ZipOutputStream(o)
    fs.foreach(write(z, _))
    z.close()
  }

  def write(z: ZipOutputStream, f: Fixture[F]): Unit = {
    val p = new ItcParameters(f.src, f.odp, f.ocp, f.tep, f.ins)
    val recipe = prepareRecipe(p, f)
    val result = cookRecipe(recipe).foreach { r =>
        for {
          (g, gix)  <- r.chartGroups.zipWithIndex
          ct        <- SpcChartType.Values
          dt        <- SpcDataType.Values
          chart     <- g.chart(ct)
          series    =  chart.allSeries(dt)
        } yield
          write(z, f, gix, ct, dt, series)
      }
    }

  def write(z: ZipOutputStream, f: Fixture[F], g: GroupIx, ct: SpcChartType, dt: SpcDataType, ss: List[SpcSeriesData]): Unit =
    ss.zipWithIndex.foreach { case (s: SpcSeriesData, i) =>
      val data = Data.fromSeries(s).compress
      val e = new ZipEntry(BaselineId(f, g, ct, dt, i).filename)
      z.putNextEntry(e)
      z.write(writeData(data).getBytes)
      z.closeEntry()
    }

  def writeData(data: Data): String = {
    val writer = new StringBuilder()
    data.values.foreach({case (x,y) =>
      writer.append(f"$x%.3f,$y%.3f\n")
    })
    writer.toString()
  }
  //=======
  // TODO: move this to baseline
  //======= READ BASELINE DATA

  def read(file: String): BaselineNG = {

    import scala.collection.JavaConversions._

    def readEntry(z: InputStream, data: Array[Byte], offset: Int): Array[Byte] =
      z.read(data, offset, data.length - offset) match {
        case r if r > 0 => readEntry(z, data, offset + r)
        case _          => data // done
      }

    val f = new ZipFile(file)
    f.entries().map { e =>
      val in    = f.getInputStream(e)
      val data  = new Array[Byte](e.getSize.toInt)
      readEntry(in, data, 0)
      in.close()
      BaselineId(e.getName) -> Data(DatFile.fromUserSpectrum(new String(data)))
    }.toMap

  }

  //=======

  private def cook(f: Fixture[F]): Option[ItcSpectroscopyResult] = {
    val p      = new ItcParameters(f.src, f.odp, f.ocp, f.tep, f.ins)
    val recipe = prepareRecipe(p, f)
    cookRecipe(recipe)
  }

  private def cookRecipe(r: Recipe): Option[ItcSpectroscopyResult] = r match {
    case r: SpectroscopyRecipe      => Some(r.serviceResult(r.calculateSpectroscopy()))
    case r: SpectroscopyArrayRecipe => Some(r.serviceResult(r.calculateSpectroscopy()))
    case _                          => None
  }

  private def baselineData(f: Fixture[F], g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx): Option[Data] =
    baseline.get(BaselineId(f, g, c, d, s))

  private def seriesData(r: ItcSpectroscopyResult): Option[List[SpcSeriesData]] =
    r.series(curChart, curData, curGroup)

  private def seriesData(r: ItcSpectroscopyResult, g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx): Option[SpcSeriesData] =
    r.series(c, d, g).map(_(s))


  // ======== ITERATOR
  // this is basically a iterator
  final case class Index(f: Fixture[F], r: ItcSpectroscopyResult, g: GroupIx, c: SpcChartType, d: SpcDataType) {

    def next(): Option[Index] = {

      // TODO: collapse this into a single for statement?
      val nd = next(d)
      val nc = nd match {
        case Some(_) => Some(c)
        case None    => next(c)
      }
      val ng = nc match {
        case Some(_) => Some(g)
        case None    => next(g)
      }

      for {
        dd <- nd.orElse(Some(SpcDataType.Values.head))    // reset to first value and advance chart
        cc <- nc.orElse(Some(SpcChartType.Values.head))   // reset to first values and advance group
        gg <- ng                                          // if we can't advance group anymore we're done -> next fixture
      } yield Index(f, r, gg, cc, dd)

    }

    // note: it is possible that this fixture does not contain the data referenced by chart type/data type
    lazy val data: Option[List[Data]] = for { // TODO: use NEL?
      gg <- r.group(g)
      cc <- gg.chart(c)
      dd <- cc.allSeries(d) match { // TODO: turn empty lists, i.e. no data series for this iter available into a None
        case List() => None         // TODO: should allSeries() do that at a lower level??
        case l      => Some(l)
      }
    } yield dd.map(Data.fromSeries)

    lazy val baseline: Option[List[Data]] = for {
      dd <- data
      ss =  dd.indices
    } yield ss.flatMap(baselineData(f, g, c, d, _)).toList

    private def next(g: GroupIx) = g + 1 match {
      case ng if ng == r.chartGroups.length => None
      case ng => Some(ng)
    }
    private def next(c: SpcChartType) =
      nextInList(SpcChartType.Values, c)
    private def next(d: SpcDataType)  =
      nextInList(SpcDataType.Values, d)
    private def nextInList[A](l: List[A], e: A) = l.indexOf(e) + 1 match {
      case ni if ni == l.length => None
      case ni                  => Some(l(ni))
    }
  }

  object Index {
    def apply(f: Fixture[F], r: ItcSpectroscopyResult) =
      new Index(f, r, 0, SpcChartType.Values.head, SpcDataType.Values.head)
  }
  // ======== END OF ITERATOR

  // ======== CHECK FOR DIFF
  // Checks if the current data matches the baseline. Will return None if baseline size does not match, true/false else.
  private def isDiff(cur: Index): Option[Boolean] = for {
      d <- cur.data
      b <- cur.baseline
      if d.length == b.length
      d2 = d.map(_.compress)
    } yield d2.zip(b).forall { case (d0,d1) => d0 ~= d1 } // TODO define ~= specific to this use case?

  // ======== FIND NEXT DIFF
  @tailrec private def nextDiff(fs: List[Fixture[F]], cur: Index): Option[Index] = {

    // next fixture (if any)
    def nextF() = fs.indexOf(cur.f) + 1 match {
      case i if i == fs.length => None
      case i                   => Some(fs(i))
    }
    // next iter (if any), this will also skip any positions for which no data is available in the result
    def next(): Option[Index] = cur.next() match {
      case None                 => nextF().map(f => Index(f, cook(f).get)) // TODO: deal with get() on cook()
      case Some(n)              => Some(n) //if (n.data.isDefined) Some(n) else next()
    }
    // check for difference?
    def diff(i: Index) = isDiff(i) match {
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

  private def view(i: Index): Unit = {
    info.show(i)
    curIndex = i
    val dataSeries: List[SpcSeriesData]     = seriesData(i.r).getOrElse(List())
    val baselineSeries: List[SpcSeriesData] = seriesData(i.r).map { _.zipWithIndex.map { case (s, ix) =>
      // create series based on existing one with new data
      val bline = baselineData(i.f, curGroup, curChart, curData, ix).get // TODO: deal with get
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
    cook(f).foreach { r =>

      val dataSeries: List[SpcSeriesData]     = seriesData(r).getOrElse(List())
      val baselineSeries: List[SpcSeriesData] = seriesData(r).map { _.zipWithIndex.map { case (s, ix) =>
        // create series based on existing one with new data
        val bline = baselineData(f, curGroup, curChart, curData, ix).get // TODO: deal with get
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

  // TODO: This can become part of Fixture??, no, recipes are not in shared! move this to itc?
  def prepareRecipe(p: ItcParameters, f: Fixture[_]) = f.ins match {
    case i: AcquisitionCamParameters  => new AcqCamRecipe(p, i)
    case i: Flamingos2Parameters      => new Flamingos2Recipe(p, i)
    case i: GmosParameters            => new GmosRecipe(p, i)
    case i: GnirsParameters           => new GnirsRecipe(p, i)
    case i: GsaoiParameters           => new GsaoiRecipe(p, i)
    case i: MichelleParameters        => new MichelleRecipe(p, i)
    case i: NifsParameters            => new NifsRecipe(p, i)
    case i: NiriParameters            => new NiriRecipe(p, i)
    case i: TRecsParameters           => new TRecsRecipe(p, i)
  }

  final class InfoPanel extends GridBagPanel {

    import scala.swing.GridBagPanel.Fill._
    import scala.swing.GridBagPanel.Anchor._

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

    def show(i: Index) {
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
