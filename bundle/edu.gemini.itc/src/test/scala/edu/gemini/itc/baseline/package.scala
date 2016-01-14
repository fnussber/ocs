package edu.gemini.itc

import edu.gemini.itc.baseline.util.{Data, Fixture}
import edu.gemini.itc.shared._

package object baseline {

  type BaselineNG = Map[BaselineId, Data]
  type GroupIx  = Int
  type SeriesIx = Int

  final case class BaselineId(filename: String)
  object BaselineId {
    def apply[F <: InstrumentDetails](f: Fixture[F], g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx) = new BaselineId(s"${f.hash}.$g.$c.$d.$s.txt")
  }

}
