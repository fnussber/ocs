package edu.gemini.itc.gnirs;

import edu.gemini.itc.operation.*;
import edu.gemini.itc.shared.*;
import edu.gemini.itc.web.HtmlPrinter;
import edu.gemini.itc.web.ITCRequest;
import edu.gemini.spModel.core.Site;
import org.jfree.chart.ChartColor;

import java.io.PrintWriter;
import java.util.Calendar;

/**
 * This class performs the calculations for Gnirs used for imaging.
 */
public final class GnirsRecipe extends RecipeBase {

    private Calendar now = Calendar.getInstance();
    private String _header = new StringBuffer("# GNIRS ITC: " + now.getTime() + "\n").toString();

    private String sigSpec, backSpec, singleS2N, finalS2N;
    private SpecS2NLargeSlitVisitor specS2N;

    // Parameters from the web page.
    private final SourceDefinition _sdParameters;
    private final ObservationDetails _obsDetailParameters;
    private final ObservingConditions _obsConditionParameters;
    private final GnirsParameters _gnirsParameters;
    private final TelescopeDetails _telescope;
    private final PlottingDetails _plotParameters;

    private VisitableSampledSpectrum signalOrder3, signalOrder4, signalOrder5,
            signalOrder6, signalOrder7, signalOrder8;
    private VisitableSampledSpectrum backGroundOrder3, backGroundOrder4,
            backGroundOrder5, backGroundOrder6, backGroundOrder7,
            backGroundOrder8;
    private VisitableSampledSpectrum finalS2NOrder3, finalS2NOrder4,
            finalS2NOrder5, finalS2NOrder6, finalS2NOrder7, finalS2NOrder8;

    /**
     * Constructs a GnirsRecipe by parsing a Multipart servlet request.
     *
     * @param r   Servlet request containing form data from ITC web page.
     * @param out Results will be written to this PrintWriter.
     * @throws Exception on failure to parse parameters.
     */
    public GnirsRecipe(ITCMultiPartParser r, PrintWriter out) {
        super(out);
        // Read parameters from the four main sections of the web page.
        _sdParameters = ITCRequest.sourceDefinitionParameters(r);
        _obsDetailParameters = ITCRequest.observationParameters(r);
        _obsConditionParameters = ITCRequest.obsConditionParameters(r);
        _gnirsParameters = new GnirsParameters(r);
        _telescope = ITCRequest.teleParameters(r);
        _plotParameters = ITCRequest.plotParamters(r);
    }

    /**
     * Constructs a GnirsRecipe given the parameters. Useful for testing.
     */
    public GnirsRecipe(SourceDefinition sdParameters,
                       ObservationDetails obsDetailParameters,
                       ObservingConditions obsConditionParameters,
                       GnirsParameters gnirsParameters, TelescopeDetails telescope,
                       PlottingDetails plotParameters,
                       PrintWriter out)

    {
        super(out);
        _sdParameters = sdParameters;
        _obsDetailParameters = obsDetailParameters;
        _obsConditionParameters = obsConditionParameters;
        _gnirsParameters = gnirsParameters;
        _telescope = telescope;
//        _altairParameters = altairParameters;// REL-472: Commenting out Altair option for now
        _plotParameters = plotParameters;
    }

    /**
     * Performes recipe calculation and writes results to a cached PrintWriter
     * or to System.out.
     *
     * @throws Exception A recipe calculation can fail in many ways, missing data
     *                   files, incorrectly-formatted data files, ...
     */
    public void writeOutput() {
        _println("");

        // This object is used to format numerical strings.
        FormatStringWriter device = new FormatStringWriter();
        device.setPrecision(2); // Two decimal places
        device.clear();

        // Module 1b
        // Define the source energy (as function of wavelength).
        //
        // inputs: instrument, SED
        // calculates: redshifted SED
        // output: redshifteed SED
        Gnirs instrument;

        //instrument = new GnirsSouth(_gnirsParameters, _obsDetailParameters);
        instrument = new GnirsNorth(_gnirsParameters, _obsDetailParameters);   // Added on 2/27/2014 (see REL-480)

        if (_sdParameters.getDistributionType().equals(SourceDefinition.Distribution.ELINE))
            // *25 b/c of increased resolutuion of transmission files
            if (_sdParameters.getELineWidth() < (3E5 / (_sdParameters.getELineWavelength() * 1000 * 25))) {
                throw new RuntimeException(
                        "Please use a model line width > 0.04 nm (or "
                                + (3E5 / (_sdParameters.getELineWavelength() * 1000 * 25))
                                + " km/s) to avoid undersampling of the line profile when convolved with the transmission response");
            }

        double pixel_size = instrument.getPixelSize();
        double ap_diam = 0;

        // Calculate image quality
        double im_qual;
        double uncorrected_im_qual = 0.;

        final ImageQualityCalculatable IQcalc = ImageQualityCalculationFactory.getCalculationInstance(_sdParameters, _obsConditionParameters, _telescope, instrument);
        IQcalc.calculate();
        im_qual = IQcalc.getImageQuality();


        // Get the summed source and sky
        final SEDFactory.SourceResult calcSource = SEDFactory.calculate(instrument, Site.GN, ITCConstants.NEAR_IR, _sdParameters, _obsConditionParameters, _telescope, _plotParameters);
        final VisitableSampledSpectrum sed = calcSource.sed;
        final VisitableSampledSpectrum sky = calcSource.sky;
        double sed_integral = sed.getIntegral();
        double sky_integral = sky.getIntegral();

        // Calculate the Fraction of source in the aperture
        final SourceFraction SFcalc = SourceFractionFactory.calculate(_sdParameters, _obsDetailParameters, instrument, im_qual);

        // this will be the core for an altair source; unchanged for non altair.
        if (_obsDetailParameters.getMethod().isImaging()) {
            _print(SFcalc.getTextResult(device));
            _println(IQcalc.getTextResult(device));
            _println("Sky subtraction aperture = "
                    + _obsDetailParameters.getSkyApertureDiameter()
                    + " times the software aperture.\n");
            _println(IQcalc.getTextResult(device));
        }

        // Calculate the Peak Pixel Flux
        final double peak_pixel_count = PeakPixelFlux.calculate(instrument, _sdParameters, _obsDetailParameters, SFcalc, im_qual, sed_integral, sky_integral);

        // In this version we are bypassing morphology modules 3a-5a.
        // i.e. the output morphology is same as the input morphology.
        // Might implement these modules at a later time.
        int number_exposures = _obsDetailParameters.getNumExposures();
        double frac_with_source = _obsDetailParameters.getSourceFraction();
        double dark_current = instrument.getDarkCurrent();
        double exposure_time = _obsDetailParameters.getExposureTime();
        double read_noise = instrument.getReadNoise();

        // report error if this does not come out to be an integer
        checkSourceFraction(number_exposures, frac_with_source);

        // ObservationMode Imaging or spectroscopy

        if (_obsDetailParameters.getMethod().isSpectroscopy()) {

            SlitThroughput st;
            SlitThroughput st_halo = null;

            if (!_obsDetailParameters.isAutoAperture()) {
                st = new SlitThroughput(im_qual,
                        _obsDetailParameters.getApertureDiameter(),
                        pixel_size, _gnirsParameters.getFPMask());
                st_halo = new SlitThroughput(uncorrected_im_qual,
                        _obsDetailParameters.getApertureDiameter(), pixel_size,
                        _gnirsParameters.getFPMask());
                _println("software aperture extent along slit = "
                        + device.toString(_obsDetailParameters
                        .getApertureDiameter()) + " arcsec");
            } else {
                st = new SlitThroughput(im_qual, pixel_size, _gnirsParameters.getFPMask());
                st_halo = new SlitThroughput(uncorrected_im_qual, pixel_size, _gnirsParameters.getFPMask());

                switch (_sdParameters.getProfileType()) {
                    case UNIFORM:
                        _println("software aperture extent along slit = "
                                + device.toString(1 / _gnirsParameters
                                .getFPMask()) + " arcsec");
                        break;
                    case POINT:
                        _println("software aperture extent along slit = "
                                + device.toString(1.4 * im_qual) + " arcsec");
                        break;
                }

            }

            if (!_sdParameters.isUniform()) {
                _println("fraction of source flux in aperture = "
                        + device.toString(st.getSlitThroughput()));
            }


            _println("derived image size(FWHM) for a point source = "
                    + device.toString(im_qual) + "arcsec\n");

            _println("Sky subtraction aperture = "
                    + _obsDetailParameters.getSkyApertureDiameter()
                    + " times the software aperture.");

            _println("");
            _println("Requested total integration time = "
                    + device.toString(exposure_time * number_exposures)
                    + " secs, of which "
                    + device.toString(exposure_time * number_exposures
                    * frac_with_source) + " secs is on source.");

            _print("<HR align=left SIZE=3>");

            ap_diam = st.getSpatialPix(); // ap_diam really Spec_Npix on

            // REL-472: Since GNIRS does not have perfect optics, the PSF delivered by Altair should then be
            // convolved with a ~0.10" Gaussian to reproduce the ~0.12" images which are measured under optimal conditions.
            // XXX TODO: Not sure how to do this. See comments in REL-472


            double spec_source_frac = st.getSlitThroughput();
            double halo_spec_source_frac = 0;
            if (st_halo != null) halo_spec_source_frac = st_halo.getSlitThroughput();

            // For the usb case we want the resolution to be determined by the
            // slit width and not the image quality for a point source.
            if (_sdParameters.isUniform()) {
                im_qual = 10000;
                if (_obsDetailParameters.isAutoAperture()) {
                    ap_diam = new Double(1 / (_gnirsParameters.getFPMask() * pixel_size) + 0.5).intValue();
                    spec_source_frac = 1;
                } else {
                    spec_source_frac = _gnirsParameters.getFPMask() * ap_diam * pixel_size;
                }
            }

            specS2N = new SpecS2NLargeSlitVisitor(
                    _gnirsParameters.getFPMask(), pixel_size,
                    instrument.getSpectralPixelWidth() / instrument.getOrder(),
                    instrument.getObservingStart(),
                    instrument.getObservingEnd(),
                    instrument.getGratingDispersion_nm(),
                    instrument.getGratingDispersion_nmppix(),
                    instrument.getGratingResolution(),
                    spec_source_frac,
                    im_qual, ap_diam,
                    number_exposures,
                    frac_with_source,
                    exposure_time,
                    dark_current,
                    read_noise,
                    _obsDetailParameters.getSkyApertureDiameter());

            // DEBUG
            // _println("RESOLUTION DEBUGGING");
            // _println("gratingDispersion_nm: " +
            // instrument.getGratingDispersion_nm());
            // _println("source sampling: " + sed.getSampling());

            _println("<p style=\"page-break-inside: never\">");

            specS2N.setDetectorTransmission(instrument
                    .getDetectorTransmision());

            if (instrument.XDisp_IsUsed()) {
                // sed.trim(700, 2533);
                VisitableSampledSpectrum sedOrder3 = (VisitableSampledSpectrum) sed.clone();
                VisitableSampledSpectrum sedOrder4 = (VisitableSampledSpectrum) sed.clone();
                VisitableSampledSpectrum sedOrder5 = (VisitableSampledSpectrum) sed.clone();
                VisitableSampledSpectrum sedOrder6 = (VisitableSampledSpectrum) sed.clone();
                VisitableSampledSpectrum sedOrder7 = (VisitableSampledSpectrum) sed.clone();
                VisitableSampledSpectrum sedOrder8 = (VisitableSampledSpectrum) sed.clone();
                // sky.trim(700, 2533);
                VisitableSampledSpectrum skyOrder3 = (VisitableSampledSpectrum) sky.clone();
                VisitableSampledSpectrum skyOrder4 = (VisitableSampledSpectrum) sky.clone();
                VisitableSampledSpectrum skyOrder5 = (VisitableSampledSpectrum) sky.clone();
                VisitableSampledSpectrum skyOrder6 = (VisitableSampledSpectrum) sky.clone();
                VisitableSampledSpectrum skyOrder7 = (VisitableSampledSpectrum) sky.clone();
                VisitableSampledSpectrum skyOrder8 = (VisitableSampledSpectrum) sky.clone();

                double trimCenter;
                if (instrument.getGrating().equals(_gnirsParameters.G110))
                    trimCenter = _gnirsParameters.getUnXDispCentralWavelength();
                else
                    trimCenter = 2200.0;

                sedOrder3.accept(instrument.getGratingOrderNTransmission(3));
                sedOrder3.trim(
                        trimCenter
                                * 3
                                / 3
                                - (instrument.getGratingDispersion_nmppix()
                                / 3 * instrument.DETECTOR_PIXELS / 2),
                        trimCenter
                                * 3
                                / 3
                                + (instrument
                                .getGratingDispersion_nmppix()
                                / 3
                                * instrument.DETECTOR_PIXELS / 2));
                skyOrder3
                        .accept(instrument.getGratingOrderNTransmission(3));
                skyOrder3
                        .trim(trimCenter
                                        * 3
                                        / 3
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 3 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 3
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 3
                                        * instrument.DETECTOR_PIXELS / 2));
                sedOrder4
                        .accept(instrument.getGratingOrderNTransmission(4));
                sedOrder4
                        .trim(trimCenter
                                        * 3
                                        / 4
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 4 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 4
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 4
                                        * instrument.DETECTOR_PIXELS / 2));
                skyOrder4
                        .accept(instrument.getGratingOrderNTransmission(4));
                skyOrder4
                        .trim(trimCenter
                                        * 3
                                        / 4
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 4 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 4
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 4
                                        * instrument.DETECTOR_PIXELS / 2));
                sedOrder5
                        .accept(instrument.getGratingOrderNTransmission(5));
                sedOrder5
                        .trim(trimCenter
                                        * 3
                                        / 5
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 5 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 5
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 5
                                        * instrument.DETECTOR_PIXELS / 2));
                skyOrder5
                        .accept(instrument.getGratingOrderNTransmission(5));
                skyOrder5
                        .trim(trimCenter
                                        * 3
                                        / 5
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 5 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 5
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 5
                                        * instrument.DETECTOR_PIXELS / 2));
                sedOrder6
                        .accept(instrument.getGratingOrderNTransmission(6));
                sedOrder6
                        .trim(trimCenter
                                        * 3
                                        / 6
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 6 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 6
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 6
                                        * instrument.DETECTOR_PIXELS / 2));
                skyOrder6
                        .accept(instrument.getGratingOrderNTransmission(6));
                skyOrder6
                        .trim(trimCenter
                                        * 3
                                        / 6
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 6 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 6
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 6
                                        * instrument.DETECTOR_PIXELS / 2));
                sedOrder7
                        .accept(instrument.getGratingOrderNTransmission(7));
                sedOrder7
                        .trim(trimCenter
                                        * 3
                                        / 7
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 7 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 7
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 7
                                        * instrument.DETECTOR_PIXELS / 2));
                skyOrder7
                        .accept(instrument.getGratingOrderNTransmission(7));
                skyOrder7
                        .trim(trimCenter
                                        * 3
                                        / 7
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 7 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 7
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 7
                                        * instrument.DETECTOR_PIXELS / 2));
                sedOrder8
                        .accept(instrument.getGratingOrderNTransmission(8));
                sedOrder8
                        .trim(trimCenter
                                        * 3
                                        / 8
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 8 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 8
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 8
                                        * instrument.DETECTOR_PIXELS / 2));
                skyOrder8
                        .accept(instrument.getGratingOrderNTransmission(8));
                skyOrder8
                        .trim(trimCenter
                                        * 3
                                        / 8
                                        - (instrument.getGratingDispersion_nmppix()
                                        / 8 * instrument.DETECTOR_PIXELS / 2),
                                trimCenter
                                        * 3
                                        / 8
                                        + (instrument
                                        .getGratingDispersion_nmppix()
                                        / 8
                                        * instrument.DETECTOR_PIXELS / 2));

                specS2N.setSourceSpectrum(sedOrder3);
                specS2N.setBackgroundSpectrum(skyOrder3);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 3);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 3);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 3);

                specS2N.setStartWavelength(sedOrder3.getStart());
                specS2N.setEndWavelength(sedOrder3.getEnd());

                sed.accept(specS2N);

                signalOrder3 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder3 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder4);
                specS2N.setBackgroundSpectrum(skyOrder4);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 4);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 4);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 4);

                specS2N.setStartWavelength(sedOrder4.getStart());
                specS2N.setEndWavelength(sedOrder4.getEnd());

                sed.accept(specS2N);

                signalOrder4 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder4 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder5);
                specS2N.setBackgroundSpectrum(skyOrder5);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 5);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 5);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 5);

                specS2N.setStartWavelength(sedOrder5.getStart());
                specS2N.setEndWavelength(sedOrder5.getEnd());

                sed.accept(specS2N);

                signalOrder5 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder5 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder6);
                specS2N.setBackgroundSpectrum(skyOrder6);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 6);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 6);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 6);

                specS2N.setStartWavelength(sedOrder6.getStart());
                specS2N.setEndWavelength(sedOrder6.getEnd());

                sed.accept(specS2N);

                signalOrder6 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder6 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder7);
                specS2N.setBackgroundSpectrum(skyOrder7);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 7);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 7);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 7);

                specS2N.setStartWavelength(sedOrder7.getStart());
                specS2N.setEndWavelength(sedOrder7.getEnd());

                sed.accept(specS2N);

                signalOrder7 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder7 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder8);
                specS2N.setBackgroundSpectrum(skyOrder8);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 8);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 8);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 8);

                specS2N.setStartWavelength(sedOrder8.getStart());
                specS2N.setEndWavelength(sedOrder8.getEnd());

                sed.accept(specS2N);

                signalOrder8 = (VisitableSampledSpectrum) specS2N
                        .getSignalSpectrum().clone();
                backGroundOrder8 = (VisitableSampledSpectrum) specS2N
                        .getBackgroundSpectrum().clone();

                final ITCChart chart1 = new ITCChart("Signal and Background in software aperture of " + ap_diam + " pixels", "Wavelength (nm)", "e- per exposure per spectral pixel", _plotParameters);
                chart1.addArray(signalOrder3.getData(), "Signal Order 3", ChartColor.DARK_RED);
                chart1.addArray(backGroundOrder3.getData(), "SQRT(Background) Order 3 ", ChartColor.VERY_LIGHT_RED);
                chart1.addArray(signalOrder4.getData(), "Signal Order 4", ChartColor.DARK_BLUE);
                chart1.addArray(backGroundOrder4.getData(), "SQRT(Background)  Order 4", ChartColor.VERY_LIGHT_BLUE);
                chart1.addArray(signalOrder5.getData(), "Signal Order 5", ChartColor.DARK_GREEN);
                chart1.addArray(backGroundOrder5.getData(), "SQRT(Background)  Order 5", org.jfree.chart.ChartColor.VERY_LIGHT_GREEN);
                chart1.addArray(signalOrder6.getData(), "Signal Order 6", ChartColor.DARK_MAGENTA);
                chart1.addArray(backGroundOrder6.getData(), "SQRT(Background) Order 6", ChartColor.VERY_LIGHT_MAGENTA);
                chart1.addArray(signalOrder7.getData(), "Signal Order 7", ChartColor.black);
                chart1.addArray(backGroundOrder7.getData(), "SQRT(Background) Order 7", ChartColor.lightGray);
                chart1.addArray(signalOrder8.getData(), "Signal Order 8", ChartColor.DARK_CYAN);
                chart1.addArray(backGroundOrder8.getData(), "SQRT(Background) Order 8", ChartColor.VERY_LIGHT_CYAN);
                _println(chart1.getBufferedImage(), "SigAndBack");
                _println("");

                sigSpec = _printSpecTag("ASCII signal spectrum");
                backSpec = _printSpecTag("ASCII background spectrum");

                specS2N.setSourceSpectrum(sedOrder3);
                specS2N.setBackgroundSpectrum(skyOrder3);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 3);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 3);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 3);

                specS2N.setStartWavelength(sedOrder3.getStart());
                specS2N.setEndWavelength(sedOrder3.getEnd());

                sed.accept(specS2N);

                finalS2NOrder3 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder4);
                specS2N.setBackgroundSpectrum(skyOrder4);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 4);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 4);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 4);

                specS2N.setStartWavelength(sedOrder4.getStart());
                specS2N.setEndWavelength(sedOrder4.getEnd());

                sed.accept(specS2N);

                finalS2NOrder4 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder5);
                specS2N.setBackgroundSpectrum(skyOrder5);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 5);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 5);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 5);

                specS2N.setStartWavelength(sedOrder5.getStart());
                specS2N.setEndWavelength(sedOrder5.getEnd());

                sed.accept(specS2N);

                finalS2NOrder5 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder6);
                specS2N.setBackgroundSpectrum(skyOrder6);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 6);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 6);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 6);

                specS2N.setStartWavelength(sedOrder6.getStart());
                specS2N.setEndWavelength(sedOrder6.getEnd());

                sed.accept(specS2N);

                finalS2NOrder6 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder7);
                specS2N.setBackgroundSpectrum(skyOrder7);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 7);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 7);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 7);

                specS2N.setStartWavelength(sedOrder7.getStart());
                specS2N.setEndWavelength(sedOrder7.getEnd());

                sed.accept(specS2N);

                finalS2NOrder7 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                specS2N.setSourceSpectrum(sedOrder8);
                specS2N.setBackgroundSpectrum(skyOrder8);

                specS2N.setGratingDispersion_nmppix(instrument
                        .getGratingDispersion_nmppix() / 8);
                specS2N.setGratingDispersion_nm(instrument
                        .getGratingDispersion_nm() / 8);
                specS2N.setSpectralPixelWidth(instrument
                        .getSpectralPixelWidth() / 8);

                specS2N.setStartWavelength(sedOrder8.getStart());
                specS2N.setEndWavelength(sedOrder8.getEnd());

                sed.accept(specS2N);

                finalS2NOrder8 = (VisitableSampledSpectrum) specS2N
                        .getFinalS2NSpectrum().clone();

                final ITCChart chart2 = new ITCChart("Final S/N", "Wavelength (nm)", "Signal / Noise per spectral pixel", _plotParameters);
                chart2.addArray(finalS2NOrder3.getData(), "Final S/N Order 3", ChartColor.DARK_RED);
                chart2.addArray(finalS2NOrder4.getData(), "Final S/N Order 4", ChartColor.DARK_BLUE);
                chart2.addArray(finalS2NOrder5.getData(), "Final S/N Order 5", ChartColor.DARK_GREEN);
                chart2.addArray(finalS2NOrder6.getData(), "Final S/N Order 6", ChartColor.DARK_MAGENTA);
                chart2.addArray(finalS2NOrder7.getData(), "Final S/N Order 7", ChartColor.black);
                chart2.addArray(finalS2NOrder8.getData(), "Final S/N Order 8", ChartColor.DARK_CYAN);
                _println(chart2.getBufferedImage(), "Sig2N");
                _println("");

                finalS2N = _printSpecTag("Final S/N ASCII data");


            } else {

                // Added 20100924. As far as I can tell the grating
                // transmission was not applied
                // unless you were in cross-dispersed mode. I'm adding it
                // here. MD
                sed.accept(instrument
                        .getGratingOrderNTransmission(instrument.getOrder()));

                specS2N.setSourceSpectrum(sed);
                specS2N.setBackgroundSpectrum(sky);
                specS2N.setHaloImageQuality(uncorrected_im_qual);

//                // REL-472: Commenting out Altair option for now
//                if (_altairParameters.altairIsUsed()) {
//                    specS2N.setSpecHaloSourceFraction(halo_spec_source_frac);
//                }
//                else {
                specS2N.setSpecHaloSourceFraction(0.0);
//                }

                sed.accept(specS2N);

                final ITCChart chart1 = new ITCChart("Signal and Background in software aperture of " + ap_diam + " pixels", "Wavelength (nm)", "e- per exposure per spectral pixel", _plotParameters);
                chart1.addArray(specS2N.getSignalSpectrum().getData(), "Signal ");
                chart1.addArray(specS2N.getBackgroundSpectrum().getData(), "SQRT(Background)  ");
                _println(chart1.getBufferedImage(), "SigAndBack");
                _println("");

                sigSpec = _printSpecTag("ASCII signal spectrum");
                backSpec = _printSpecTag("ASCII background spectrum");

                final ITCChart chart2 = new ITCChart("Intermediate Single Exp and Final S/N", "Wavelength (nm)", "Signal / Noise per spectral pixel", _plotParameters);
                chart2.addArray(specS2N.getExpS2NSpectrum().getData(), "Single Exp S/N");
                chart2.addArray(specS2N.getFinalS2NSpectrum().getData(), "Final S/N  ");
                _println(chart2.getBufferedImage(), "Sig2N");
                _println("");

                singleS2N = _printSpecTag("Single Exposure S/N ASCII data");
                finalS2N = _printSpecTag("Final S/N ASCII data");
            }

        } else {

            final ImagingS2NCalculatable IS2Ncalc = ImagingS2NCalculationFactory.getCalculationInstance(_obsDetailParameters, instrument, SFcalc, sed_integral, sky_integral);
            IS2Ncalc.calculate();
            _println(IS2Ncalc.getTextResult(device));
            device.setPrecision(0); // NO decimal places
            device.clear();

            _println("");
            _println("The peak pixel signal + background is "
                    + device.toString(peak_pixel_count) + ". ");

            if (peak_pixel_count > (.95 * instrument.getWellDepth()))
                _println("Warning: peak pixel may be saturating the (binned) CCD full well of "
                        + .95 * instrument.getWellDepth());

            if (peak_pixel_count > (.95 * instrument.getADSaturation() * instrument
                    .getLowGain()))
                _println("Warning: peak pixel may be saturating the low gain setting of "
                        + .95
                        * instrument.getADSaturation()
                        * instrument.getLowGain());

            if (peak_pixel_count > (.95 * instrument.getADSaturation() * instrument
                    .getHighGain()))
                _println("Warning: peak pixel may be saturating the high gain setting "
                        + .95
                        * instrument.getADSaturation()
                        * instrument.getHighGain());

        }

        _println("");
        device.setPrecision(2); // TWO decimal places
        device.clear();

        // _println("");
        _print("<HR align=left SIZE=3>");

        _println("<b>Input Parameters:</b>");
        _println("Instrument: " + instrument.getName() + "\n");
        _println(HtmlPrinter.printParameterSummary(_sdParameters));
        _println(instrument.toString());
        _println(HtmlPrinter.printParameterSummary(_telescope));
        _println(HtmlPrinter.printParameterSummary(_obsConditionParameters));
        _println(HtmlPrinter.printParameterSummary(_obsDetailParameters));
        if (_obsDetailParameters.getMethod().isSpectroscopy()) {
            _println(HtmlPrinter.printParameterSummary(_plotParameters));
        }

        if (_obsDetailParameters.getMethod().isSpectroscopy()) { // 49 ms
            if (instrument.XDisp_IsUsed()) {
                _println(signalOrder3, _header, sigSpec);
                _println(signalOrder4, _header, sigSpec);
                _println(signalOrder5, _header, sigSpec);
                _println(signalOrder6, _header, sigSpec);
                _println(signalOrder7, _header, sigSpec);
                _println(signalOrder8, _header, sigSpec);

                _println(backGroundOrder3, _header, backSpec);
                _println(backGroundOrder4, _header, backSpec);
                _println(backGroundOrder5, _header, backSpec);
                _println(backGroundOrder6, _header, backSpec);
                _println(backGroundOrder7, _header, backSpec);
                _println(backGroundOrder8, _header, backSpec);

                _println(finalS2NOrder3, _header, finalS2N);
                _println(finalS2NOrder4, _header, finalS2N);
                _println(finalS2NOrder5, _header, finalS2N);
                _println(finalS2NOrder6, _header, finalS2N);
                _println(finalS2NOrder7, _header, finalS2N);
                _println(finalS2NOrder8, _header, finalS2N);
            } else {
                _println(specS2N.getSignalSpectrum(), _header, sigSpec);
                _println(specS2N.getBackgroundSpectrum(), _header, backSpec);
                _println(specS2N.getExpS2NSpectrum(), _header, singleS2N);
                _println(specS2N.getFinalS2NSpectrum(), _header, finalS2N);
            }
        }
    }
}
