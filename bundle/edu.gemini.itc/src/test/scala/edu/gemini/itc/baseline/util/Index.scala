package edu.gemini.itc.baseline.util

import edu.gemini.itc.baseline._
import edu.gemini.itc.shared.{InstrumentDetails, SpcDataType, SpcChartType, ItcSpectroscopyResult}

// this is basically a iterator
final case class Index[F <: InstrumentDetails](f: Fixture[F], r: ItcSpectroscopyResult, g: GroupIx, c: SpcChartType, d: SpcDataType) {

  def next(): Option[Index[F]] = {

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
  } yield ss.flatMap(Baseline.baselineData(f, g, c, d, _)).toList

  private def next(g: GroupIx): Option[GroupIx] = g + 1 match {
    case ng if ng == r.chartGroups.length => None
    case ng => Some(ng)
  }

  private def next(c: SpcChartType): Option[SpcChartType] =
    nextInList(SpcChartType.Values, c)

  private def next(d: SpcDataType): Option[SpcDataType]  =
    nextInList(SpcDataType.Values, d)

  private def nextInList[A](l: List[A], e: A): Option[A] = l.indexOf(e) + 1 match {
    case ni if ni == l.length => None
    case ni                   => Some(l(ni))
  }
}

object Index {
  def apply[F <: InstrumentDetails](f: Fixture[F], r: ItcSpectroscopyResult) =
    new Index(f, r, 0, SpcChartType.Values.head, SpcDataType.Values.head)
}
