package edu.gemini.itc.baseline

import java.io.{FileOutputStream, InputStream}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import edu.gemini.itc.acqcam.AcqCamRecipe
import edu.gemini.itc.base.{DatFile, Recipe, SpectroscopyArrayRecipe, SpectroscopyRecipe}
import edu.gemini.itc.baseline.util.{Data, DataDiffViewer, Fixture}
import edu.gemini.itc.flamingos2.Flamingos2Recipe
import edu.gemini.itc.gmos.GmosRecipe
import edu.gemini.itc.gnirs.GnirsRecipe
import edu.gemini.itc.gsaoi.GsaoiRecipe
import edu.gemini.itc.michelle.MichelleRecipe
import edu.gemini.itc.nifs.NifsRecipe
import edu.gemini.itc.niri.NiriRecipe
import edu.gemini.itc.shared._
import edu.gemini.itc.trecs.TRecsRecipe

class Baseline {

}

object Baseline {

  lazy val baseline = read("/Users/fnussber/Downloads/GmosBaseline.zip")

  // TODO: move this to baseline
  //======= WRITE BASELINE DATA
  def write[F <: InstrumentDetails](file: String, fs: List[Fixture[F]]): Unit = {
    val o = new FileOutputStream(file)
    val z = new ZipOutputStream(o)
    fs.foreach(write(z, _))
    z.close()
  }

  def write[F <: InstrumentDetails](z: ZipOutputStream, f: Fixture[F]): Unit = {
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

  def write[F <: InstrumentDetails](z: ZipOutputStream, f: Fixture[F], g: GroupIx, ct: SpcChartType, dt: SpcDataType, ss: List[SpcSeriesData]): Unit =
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

  def baselineData[F <: InstrumentDetails](f: Fixture[F], g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx): Option[Data] =
    baseline.get(BaselineId(f, g, c, d, s))

  def cook[F <: InstrumentDetails](f: Fixture[F]): Option[ItcSpectroscopyResult] = {
    val p      = new ItcParameters(f.src, f.odp, f.ocp, f.tep, f.ins)
    val recipe = prepareRecipe(p, f)
    cookRecipe(recipe)
  }

  private def cookRecipe(r: Recipe): Option[ItcSpectroscopyResult] = r match {
    case r: SpectroscopyRecipe      => Some(r.serviceResult(r.calculateSpectroscopy()))
    case r: SpectroscopyArrayRecipe => Some(r.serviceResult(r.calculateSpectroscopy()))
    case _                          => None
  }

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



  def main(args: Array[String]) {

//    Baseline.write("/Users/fnussber/Downloads/GmosBaseline.zip", BaselineGmos.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))

    // TODO: we only support spectroscopy for now, add imaging or keep filtering here?
    new DataDiffViewer(BaselineGmos.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/GnirsBaseline.zip", BaselineGnirs.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/NifsBaseline.zip", BaselineNifs.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/NiriBaseline.zip", BaselineNiri.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))


  }

}