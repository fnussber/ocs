package edu.gemini.itc

import edu.gemini.itc.baseline.util.{Fixture, Data}
import edu.gemini.itc.shared.{SpcDataType, SpcChartType}

package object baseline {

  type BaselineNG = Map[BaselineId, Data]
  type GroupIx  = Int
  type SeriesIx = Int

  final case class BaselineId(filename: String)
  object BaselineId {
    def apply(f: Fixture[_], g: GroupIx, c: SpcChartType, d: SpcDataType, s: SeriesIx) = new BaselineId(s"${f.hash}.$g.$c.$d.$s.txt")
  }

}
