package edu.gemini.itc.shared

// ==== Source spatial profile

sealed trait SpatialProfile {
  val norm: Double
  val units: BrightnessUnit
}
final case class PointSource(norm: Double, units: BrightnessUnit) extends SpatialProfile
final case class GaussianSource(norm: Double, units: BrightnessUnit, fwhm: Double) extends SpatialProfile {
  require (fwhm >= 0.1, "Please use a Gaussian FWHM greater than 0.1")
}
final case class UniformSource(norm: Double, units: BrightnessUnit) extends SpatialProfile


// ==== Source spectral distribution

sealed trait SpectralDistribution
final case class BlackBody(temperature: Double) extends SpectralDistribution
final case class PowerLaw(index: Double) extends SpectralDistribution
final case class EmissionLine(wavelength: Double, width: Double, flux: Double, fluxUnits: String, continuum: Double, continuumUnits: String) extends SpectralDistribution
final case class UserDefined(name: String, spectrum: String) extends SpectralDistribution
sealed trait Library extends SpectralDistribution {
  val sedSpectrum: String
}
final case class LibraryStar(sedSpectrum: String) extends Library
final case class LibraryNonStar(sedSpectrum: String) extends Library


// ==== Calculation method

// TODO: We can probably get away with only IntegrationTime and S2N methods.
// TODO: The difference between spectroscopy and imaging can/should be deduced from the instrument settings!

sealed trait CalculationMethod {
  val fraction: Double
  val isIntTime: Boolean
  def isS2N: Boolean = !isIntTime
  val isImaging: Boolean
  def isSpectroscopy: Boolean = !isImaging
}
sealed trait Imaging extends CalculationMethod {
  val isImaging = true
}
sealed trait Spectroscopy extends CalculationMethod {
  val isImaging = false
}
final case class ImagingSN(exposures: Int, time: Double, fraction: Double) extends Imaging {
  val isIntTime = false
}
final case class ImagingInt(sigma: Double, expTime: Double, fraction: Double) extends Imaging {
  val isIntTime = true
}
final case class SpectroscopySN(exposures: Int, time: Double, fraction: Double) extends Spectroscopy {
  val isIntTime = false
}


// ==== Analysis method

sealed trait AnalysisMethod {
  val skyAperture: Double
}
final case class AutoAperture(skyAperture: Double) extends AnalysisMethod
final case class UserAperture(diameter: Double, skyAperture: Double) extends AnalysisMethod


// ===== IFU (GMOS & NIFS)

// TODO: Is this an analysis method (instead of the ones above?). If so, should this be reflected here?
sealed trait IfuMethod
final case class IfuSingle(offset: Double) extends IfuMethod
final case class IfuRadial(minOffset: Double, maxOffset: Double) extends IfuMethod
final case class IfuSummed(numX: Int, numY: Int, centerX: Double, centerY: Double) extends IfuMethod

