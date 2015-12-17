package edu.gemini.itc.baseline

import edu.gemini.itc.baseline.util.DataDiffViewer
import edu.gemini.itc.shared.{SpcDataType, SpcChartType, Spectroscopy}

import scalaz._, Scalaz._

class Baseline {

}

//object Iteration {
//
//  final case class Pos(g: GroupIx, c: SpcChartType, d: SpcDataType)
//
//  def nextGroup: Pos =
//}

object Baseline {

  def main(args: Array[String]) {

    // TODO: we only support spectroscopy for now, add imaging or keep filtering here?
    new DataDiffViewer("/Users/fnussber/Downloads/GmosBaseline.zip", BaselineGmos.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/GnirsBaseline.zip", BaselineGnirs.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/NifsBaseline.zip", BaselineNifs.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))
//    new DataDiffViewer("/Users/fnussber/Downloads/NiriBaseline.zip", BaselineNiri.Fixtures.filter(f => f.odp.calculationMethod.isInstanceOf[Spectroscopy]))


  }

}