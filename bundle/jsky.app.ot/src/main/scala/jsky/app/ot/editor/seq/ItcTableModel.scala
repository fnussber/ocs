package jsky.app.ot.editor.seq

import javax.swing.table.AbstractTableModel

import edu.gemini.itc.shared.{ItcImagingResult, ItcResult, ItcService}
import edu.gemini.shared.util.StringUtil
import edu.gemini.spModel.config2.ItemKey

import scala.concurrent.Future
<<<<<<< HEAD
<<<<<<< HEAD
=======
import scala.swing.Table.LabelRenderer
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
=======
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.
import scala.util.{Failure, Success}
import scalaz.Scalaz._

/** Columns in the table are defined by their header label and a function on the unique config of the row. */
<<<<<<< HEAD
<<<<<<< HEAD
case class Column(label: String, value: (ItcUniqueConfig, Future[ItcService.Result]) => AnyRef, tooltip: String = "")

object ItcTableModel {
  val PeakPixelTooltip = "Peak pixel value = signal + background"
=======
case class Column[A](label: String, value: (ItcUniqueConfig, Future[ItcService.Result]) => Object, renderer: LabelRenderer[AnyRef] = ItcTable.AnyRenderer, tooltip: String = "")
=======
case class Column(label: String, value: (ItcUniqueConfig, Future[ItcService.Result]) => AnyRef, tooltip: String = "")
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.

object ItcTableModel {
  val PeakPixelTooltip = "Peak pixel value = signal + background"
}

/** ITC tables have three types of columns: a series of header columns, then all the values that change and are
  * relevant for the different unique configs (denoted by their {{{ItemKey}}} values) and finally the ITC calculation
  * results. The static columns (headers and results) are represented by a {{{Column}}} object.
  */
sealed trait ItcTableModel extends AbstractTableModel {

  /** Defines a set of header columns for all tables. */
  val Headers = Seq(
    Column("Data Labels",     (c, r) => c.labels),
    Column("Images",          (c, r) => new java.lang.Integer(c.count),             tooltip = "Number of exposures used in S/N calculation"),
    Column("Exposure Time",   (c, r) => new java.lang.Double(c.singleExposureTime), tooltip = "Exposure time of each image [s]"),
    Column("Total Exp. Time", (c, r) => new java.lang.Double(c.totalExposureTime),  tooltip = "Total exposure time [s]"),
    Column("Source Mag",      (c, r) => sourceMag(r),                               tooltip = "Source magnitude [mag]"),
    Column("Source Band",     (c, r) => sourceBand(r),                              tooltip = "Source band")
  )
<<<<<<< HEAD
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
}

/** ITC tables have three types of columns: a series of header columns, then all the values that change and are
  * relevant for the different unique configs (denoted by their {{{ItemKey}}} values) and finally the ITC calculation
  * results. The static columns (headers and results) are represented by a {{{Column}}} object.
  */
sealed trait ItcTableModel extends AbstractTableModel {

<<<<<<< HEAD
  /** Defines a set of header columns for all tables. */
  val Headers = Seq(
    Column("Data Labels",     (c, r) => c.labels),
    Column("Images",          (c, r) => new java.lang.Integer(c.count),             tooltip = "Number of exposures used in S/N calculation"),
    Column("Exposure Time",   (c, r) => new java.lang.Double(c.singleExposureTime), tooltip = "Exposure time of each image [s]"),
    Column("Total Exp. Time", (c, r) => new java.lang.Double(c.totalExposureTime),  tooltip = "Total exposure time [s]"),
    Column("Source Mag",      (c, r) => sourceMag(r),                               tooltip = "Source magnitude [mag]"),
    Column("Source Band",     (c, r) => sourceBand(r),                              tooltip = "Source band")
  )
=======
  val headers: Seq[Column[_]]
  val keys: Seq[ItemKey]
  val results: Seq[Column[_]]
=======

  val headers:      Seq[Column]  = Headers  // override this in case different header columns are needed..
  val keys:         Seq[ItemKey]
  val results:      Seq[Column]

  val uniqueSteps:  Seq[ItcUniqueConfig]
  val res:          Seq[Future[ItcService.Result]]


  // Gets the result from the service result future (if present)
  protected def imagingCalcResult(f: Future[ItcService.Result]): Option[ItcResult] =
    for {
      futureResult  <- f.value                // unwrap future
      serviceResult <- futureResult.toOption  // unwrap try
      calcResult    <- serviceResult.toOption // unwrap validation
    } yield calcResult

  // Gets the imaging result from the service result future (if present).
  // Note that in most cases (except for GMOS) there is only one CCD in the result, but for GMOS there can be
  // 1 or 3 CCDs depending on the selected CCD manufacturer.
  protected def imagingResult(f: Future[ItcService.Result], n: Int = 0): Option[ItcImagingResult] =
    imagingCalcResult(f).flatMap { r =>
      // For GMOS ITC returns 1 or 3 different CCD results depending on the manufacturer, the simplest way to deal
      // with this is by just using n % #CCDs here, which means that if there is only one result it is repeated three
      // times, and if there are 3 results, they are shown individually as expected. All instruments other than GMOS
      // use this method with ccd index = 0.
      r.ccds(n % r.ccds.length) match {
        case img: ItcImagingResult => Some(img)
        case _                     => None
      }
    }

  protected def peakPixelFlux(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.peakPixelFlux.toInt)

  protected def singleSNRatio(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.singleSNRatio)

  protected def totalSNRatio (result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.totalSNRatio)

  // the source is the same for all CCDs, so we just always take the first one
  protected def sourceMag    (result: Future[ItcService.Result]) = imagingResult(result, 0).map(_.source.profile.norm)

  // the source is the same for all CCDs, so we just always take the first one
  protected def sourceBand   (result: Future[ItcService.Result]) = imagingResult(result, 0).map(_.source.getNormBand.name)
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.

  // ===

  override def getRowCount: Int = uniqueSteps.size

  override def getColumnCount: Int = headers.size + keys.size + results.size

  override def getValueAt(row: Int, col: Int): Object = column(col) match {
    case Some(c) => c.value(uniqueSteps(row), res(row))
    case None    => uniqueSteps(row).config.getItemValue(toKey(col))
  }

  override def getColumnName(col: Int): String = column(col) match {
    case Some(c) => c.label
    case None    => StringUtil.toDisplayName(toKey(col).getName) // create column name for key columns
  }

//  def renderer(col: Int): LabelRenderer[_] = column(col) match {
//    case Some(c) => c.renderer
//    case None    => ItcTable.AnyRenderer  // use "any" renderer for key columns
//  }

  def tooltip(col: Int): String = column(col) match {
    case Some(c) => c.tooltip
    case None    => ""                    // no tooltip for key columns
  }

  /** Gets the column description for the given {{{col}}} index. Returns {{{None}}} for dynamic key columns. */
  def column(col: Int): Option[Column] = col match {
    case c if c <  headers.size             => Some(toHeader(col))
    case c if c >= headers.size + keys.size => Some(toResult(col))
    case _                                  => None
  }

  /** Gets the ItemKey of a column (if any), this is used by the table to color code the columns. */
  def key(col: Int): Option[ItemKey] = col match {
    case c if c >= headers.size && c < headers.size + keys.size => Some(toKey(col))
    case _                                                      => None
  }
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.

<<<<<<< HEAD
  val headers:      Seq[Column]  = Headers  // override this in case different header columns are needed..
  val keys:         Seq[ItemKey]
  val results:      Seq[Column]

  val uniqueSteps:  Seq[ItcUniqueConfig]
  val res:          Seq[Future[ItcService.Result]]


  // Gets the result from the service result future (if present)
  protected def imagingCalcResult(f: Future[ItcService.Result]): Option[ItcResult] =
    for {
      futureResult  <- f.value                // unwrap future
      serviceResult <- futureResult.toOption  // unwrap try
      calcResult    <- serviceResult.toOption // unwrap validation
    } yield calcResult

  // Gets the imaging result from the service result future (if present).
  // Note that in most cases (except for GMOS) there is only one CCD in the result, but for GMOS there can be
  // 1 or 3 CCDs depending on the selected CCD manufacturer.
  protected def imagingResult(f: Future[ItcService.Result], n: Int = 0): Option[ItcImagingResult] =
    imagingCalcResult(f).flatMap { r =>
      // For GMOS ITC returns 1 or 3 different CCD results depending on the manufacturer, the simplest way to deal
      // with this is by just using n % #CCDs here, which means that if there is only one result it is repeated three
      // times, and if there are 3 results, they are shown individually as expected. All instruments other than GMOS
      // use this method with ccd index = 0.
      r.ccds(n % r.ccds.length) match {
        case img: ItcImagingResult => Some(img)
        case _                     => None
      }
    }

<<<<<<< HEAD
  protected def peakPixelFlux(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.peakPixelFlux.toInt)

  protected def singleSNRatio(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.singleSNRatio)

  protected def totalSNRatio (result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.totalSNRatio)

  // the source is the same for all CCDs, so we just always take the first one
  protected def sourceMag    (result: Future[ItcService.Result]) = imagingResult(result, 0).map(_.source.profile.norm)

  // the source is the same for all CCDs, so we just always take the first one
  protected def sourceBand   (result: Future[ItcService.Result]) = imagingResult(result, 0).map(_.source.getNormBand.name)

  // ===

  override def getRowCount: Int = uniqueSteps.size

  override def getColumnCount: Int = headers.size + keys.size + results.size

  override def getValueAt(row: Int, col: Int): Object = column(col) match {
    case Some(c) => c.value(uniqueSteps(row), res(row))
    case None    => uniqueSteps(row).config.getItemValue(toKey(col))
  }

  override def getColumnName(col: Int): String = column(col) match {
    case Some(c) => c.label
    case None    => StringUtil.toDisplayName(toKey(col).getName) // create column name for key columns
  }

  def tooltip(col: Int): String = column(col) match {
    case Some(c) => c.tooltip
    case None    => ""                    // no tooltip for key columns
  }

  /** Gets the column description for the given {{{col}}} index. Returns {{{None}}} for dynamic key columns. */
  def column(col: Int): Option[Column] = col match {
    case c if c <  headers.size             => Some(toHeader(col))
    case c if c >= headers.size + keys.size => Some(toResult(col))
    case _                                  => None
  }

  /** Gets the ItemKey of a column (if any), this is used by the table to color code the columns. */
  def key(col: Int): Option[ItemKey] = col match {
    case c if c >= headers.size && c < headers.size + keys.size => Some(toKey(col))
    case _                                                      => None
  }

  // Translate overall column index into the corresponding header, column or key value.
  private def toHeader(col: Int) = headers(col)

  private def toKey   (col: Int) = keys(col - headers.size)

  private def toResult(col: Int) = results(col - headers.size - keys.size)

=======
  // Translate overall column index into the corresponding header, column or key value.
  private def toHeader(col: Int) = headers(col)

  private def toKey   (col: Int) = keys(col - headers.size)

  private def toResult(col: Int) = results(col - headers.size - keys.size)

>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.
  // TODO: display errors/validation messages in an appropriate way in the UI, for now also print them to console
  protected def messages(f: Future[ItcService.Result]): String =
    f.value.fold("Calculating...") {
      case Failure(t) => t <| (_.printStackTrace()) |> (_.getMessage)  // "Look mummy, there's a spaceship up in the sky!"
      case Success(s) => s match {
<<<<<<< HEAD
        case scalaz.Failure(errs) => errs.mkString(", ")
        case scalaz.Success(_)    => "OK"
      }
    }
=======
  protected def peakPixelFlux(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.peakPixelFlux)

  protected def singleSNRatio(result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.singleSNRatio)

  protected def totalSNRatio (result: Future[ItcService.Result], ccd: Int = 0) = imagingResult(result, ccd).map(_.totalSNRatio)
  
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
}
=======
        case scalaz.Failure(errs) => errs.mkString(", ") <| System.out.println
        case scalaz.Success(_)    => "OK"
      }
    }
}


/** Generic ITC imaging tables model. */
sealed trait ItcImagingTableModel extends ItcTableModel
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.


/** Generic ITC imaging tables model. */
sealed trait ItcImagingTableModel extends ItcTableModel

class ItcGenericImagingTableModel(val keys: Seq[ItemKey], val uniqueSteps: Seq[ItcUniqueConfig], val res: Seq[Future[ItcService.Result]]) extends ItcImagingTableModel {
<<<<<<< HEAD
<<<<<<< HEAD
  val results = Seq(
    Column("Peak",            (c, r) => peakPixelFlux(r),         tooltip = ItcTableModel.PeakPixelTooltip),
    Column("S/N Single",      (c, r) => singleSNRatio(r)),
    Column("S/N Total",       (c, r) => totalSNRatio (r)),
=======
  val headers = ItcTableModel.Headers
  val results = Seq(
    Column("Peak",            (c, r) => peakPixelFlux(r),         ItcTable.IntRenderer,       tooltip = ItcTableModel.PeakPixelTooltip),
    Column("S/N Single",      (c, r) => singleSNRatio(r),         ItcTable.DoubleRenderer),
    Column("S/N Total",       (c, r) => totalSNRatio (r),         ItcTable.DoubleRenderer),
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
=======
  val results = Seq(
    Column("Peak",            (c, r) => peakPixelFlux(r),         tooltip = ItcTableModel.PeakPixelTooltip),
    Column("S/N Single",      (c, r) => singleSNRatio(r)),
    Column("S/N Total",       (c, r) => totalSNRatio (r)),
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.
    Column("Messages",        (c, r) => messages(r))
  )
}

/** GMOS specific ITC imaging table model. */
class ItcGmosImagingTableModel(val keys: Seq[ItemKey], val uniqueSteps: Seq[ItcUniqueConfig], val res: Seq[Future[ItcService.Result]]) extends ItcImagingTableModel {
<<<<<<< HEAD
<<<<<<< HEAD
  val results = Seq(
    Column("CCD1 Peak",       (c, r) => peakPixelFlux(r, ccd=0),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 1"),
    Column("CCD1 S/N Single", (c, r) => singleSNRatio(r, ccd=0)),
    Column("CCD1 S/N Total",  (c, r) => totalSNRatio (r, ccd=0)),
    Column("CCD2 Peak",       (c, r) => peakPixelFlux(r, ccd=1),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 2"),
    Column("CCD2 S/N Single", (c, r) => singleSNRatio(r, ccd=1)),
    Column("CCD2 S/N Total",  (c, r) => totalSNRatio (r, ccd=1)),
    Column("CCD3 Peak",       (c, r) => peakPixelFlux(r, ccd=2),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 3"),
    Column("CCD3 S/N Single", (c, r) => singleSNRatio(r, ccd=2)),
    Column("CCD3 S/N Total",  (c, r) => totalSNRatio (r, ccd=2)),
=======
  val headers = ItcTableModel.Headers
  val results = Seq(
    Column("CCD1 Peak",       (c, r) => peakPixelFlux(r, ccd=0),   ItcTable.IntRenderer,      tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 1"),
    Column("CCD1 S/N Single", (c, r) => singleSNRatio(r, ccd=0),   ItcTable.DoubleRenderer),
    Column("CCD1 S/N Total",  (c, r) => totalSNRatio (r, ccd=0),   ItcTable.DoubleRenderer),
    Column("CCD2 Peak",       (c, r) => peakPixelFlux(r, ccd=1),   ItcTable.IntRenderer,      tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 2"),
    Column("CCD2 S/N Single", (c, r) => singleSNRatio(r, ccd=1),   ItcTable.DoubleRenderer),
    Column("CCD2 S/N Total",  (c, r) => totalSNRatio (r, ccd=1),   ItcTable.DoubleRenderer),
    Column("CCD3 Peak",       (c, r) => peakPixelFlux(r, ccd=2),   ItcTable.IntRenderer,      tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 3"),
    Column("CCD3 S/N Single", (c, r) => singleSNRatio(r, ccd=2),   ItcTable.DoubleRenderer),
    Column("CCD3 S/N Total",  (c, r) => totalSNRatio (r, ccd=2),   ItcTable.DoubleRenderer),
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
=======
  val results = Seq(
    Column("CCD1 Peak",       (c, r) => peakPixelFlux(r, ccd=0),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 1"),
    Column("CCD1 S/N Single", (c, r) => singleSNRatio(r, ccd=0)),
    Column("CCD1 S/N Total",  (c, r) => totalSNRatio (r, ccd=0)),
    Column("CCD2 Peak",       (c, r) => peakPixelFlux(r, ccd=1),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 2"),
    Column("CCD2 S/N Single", (c, r) => singleSNRatio(r, ccd=1)),
    Column("CCD2 S/N Total",  (c, r) => totalSNRatio (r, ccd=1)),
    Column("CCD3 Peak",       (c, r) => peakPixelFlux(r, ccd=2),   tooltip = ItcTableModel.PeakPixelTooltip + " for CCD 3"),
    Column("CCD3 S/N Single", (c, r) => singleSNRatio(r, ccd=2)),
    Column("CCD3 S/N Total",  (c, r) => totalSNRatio (r, ccd=2)),
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.
    Column("Messages",        (c, r) => messages(r))
  )
}


/** Generic ITC spectroscopy table model. */
sealed trait ItcSpectroscopyTableModel extends ItcTableModel

class ItcGenericSpectroscopyTableModel(val keys: Seq[ItemKey], val uniqueSteps: Seq[ItcUniqueConfig], val res: Seq[Future[ItcService.Result]]) extends ItcSpectroscopyTableModel {
<<<<<<< HEAD
<<<<<<< HEAD
=======
  val headers = ItcTableModel.Headers
>>>>>>> OCSADV-295: ITC tables: tooltips, alignment, rounding of double values.
=======
>>>>>>> OCSADV-295: Extraction of source magnitude and band, added source mag and band to results.
  val results = Seq(
    Column("Messages",        (c, r) => messages(r))
  )
}