package edu.gemini.itc.operation;

import edu.gemini.itc.base.Disperser;
import edu.gemini.itc.base.SampledSpectrum;
import edu.gemini.itc.base.SampledSpectrumVisitor;
import edu.gemini.itc.base.VisitableSampledSpectrum;
import edu.gemini.itc.shared.*;

/**
 * The SpecS2NSlitVisitor is used to calculate the s2n of spectroscopy observation using a slit.
 * Note that instances of this object are "recycled" in some recipes, in particular to handle different orders
 * (GNIRS) or IFUs; this is why this class has a bunch of setters and not all variables are final. Not all
 * relevant values are set in the constructor, e.g. halo information if used with an AO system. Ideally this
 * could be changed so that instances of this class become immutable.
 */
public class SpecS2NSlitVisitor implements SampledSpectrumVisitor, SpecS2N {

    private final ObservationDetails odp;
    private final Slit slit;
    private final double throughput;
    private final double spec_frac_with_source;
    private final double spec_exp_time;
    private final double im_qual;
    private final double dark_current;
    private final double read_noise;
    private final int spec_number_exposures;

    private VisitableSampledSpectrum source_flux;
    private VisitableSampledSpectrum halo_flux;
    private VisitableSampledSpectrum background_flux;
    private VisitableSampledSpectrum spec_signal;
    private VisitableSampledSpectrum sqrt_spec_var_background;
    private VisitableSampledSpectrum spec_exp_s2n;
    private VisitableSampledSpectrum spec_final_s2n;

    private Disperser disperser;
    private double pix_width;
    private double obs_wave_low;
    private double obs_wave_high;
    private double spec_halo_source_fraction;
    private double uncorrected_im_qual;

    private int _firstCcdPixel = 0;
    private int _lastCcdPixel = -1;
    private boolean haloIsUsed = false;


    /**
     * Constructs SpecS2NVisitor with specified slit_width,
     * pixel_size, Smoothing Element, SlitThroughput, spec_Npix(sw aperture
     * size), ExpNum, frac_with_source, ExpTime .
     */
    public SpecS2NSlitVisitor(final Slit slit,
                              final Disperser disperser,
                              final double throughput,
                              final double pix_width,
                              final double obs_wave_low,
                              final double obs_wave_high,
                              final double im_qual,
                              final double read_noise,
                              final double dark_current,
                              final ObservationDetails odp) {
        this.odp                    = odp;
        this.slit                   = slit;
        this.disperser              = disperser;
        this.throughput             = throughput;
        this.pix_width              = pix_width;
        this.obs_wave_low           = obs_wave_low;
        this.obs_wave_high          = obs_wave_high;
        this.im_qual                = im_qual;
        this.dark_current           = dark_current;
        this.read_noise             = read_noise;

        // Currently SpectroscopySN is the only supported calculation method for spectroscopy.
        final CalculationMethod calcMethod = odp.calculationMethod();
        if (!(calcMethod instanceof SpectroscopyS2N)) throw new Error("Unsupported calculation method");
        this.spec_number_exposures  = ((SpectroscopyS2N) calcMethod).exposures();
        this.spec_frac_with_source  = calcMethod.sourceFraction();
        this.spec_exp_time          = calcMethod.exposureTime();

    }

    // Return index of last CCD pixel, if defined and in range
    private int lastCcdPixel(final int n) {
        if (_lastCcdPixel == -1 || _lastCcdPixel >= n) return n - 1;
        return _lastCcdPixel;
    }

    public double getImageQuality() {
        return im_qual;
    }

    /**
     * Implements the SampledSpectrumVisitor interface
     */
    public void visit(final SampledSpectrum sed) {
        // step one: do some resampling and preprocessing
        resample(slit);
        // step two: calculate S2N for single and final exposure for given slit
        calculateS2N(slit);
        // step three: calculate signal and background for single pixel
        calculateSignal(slit);
    }

    /** Resample source and background flux. (It is unknown to me why we do that, please comment if you do.) */
    private void resample(final Slit slit) {

        //calc the width of a spectral resolution element in nm
        final double res_element            = disperser.resolution(slit, im_qual);
        final double background_res_element = disperser.resolution(slit);

        //and the data size in the spectral domain
        final double res_element_data            = res_element / source_flux.getSampling();
        final double background_res_element_data = background_res_element / background_flux.getSampling();

        //use the int value of spectral_pix as a smoothing element
        int smoothing_element = new Double(res_element_data + 0.5).intValue();
        int background_smoothing_element = new Double(background_res_element_data + 0.5).intValue();
        if (smoothing_element < 1) smoothing_element = 1;
        if (background_smoothing_element < 1) background_smoothing_element = 1;
        ///////////////////////////////////////////////////////////////////////////////////////
        //  We Don't know why but using just the smoothing element is not enough to create the resolution
        //     that we expect.  Using a smoothing element of  =smoothing_element +1
        //     May need to take this out in the future.
        ///////////////////////////////////////////////////////////////////////////////////////
        smoothing_element = smoothing_element + 1;                       // Left in on 04/23/2014 (SLP)
        background_smoothing_element = background_smoothing_element + 1; // Added in on 04/23/2014 (SLP)

        source_flux.smoothY(smoothing_element);
        background_flux.smoothY(background_smoothing_element);       // Uncommented and decoupled from IQ on 04/08/2014 (SLP)

        if (haloIsUsed) {
            //calc the width of a spectral resolution element in nm
            final double halo_res_element      = disperser.resolution(slit, uncorrected_im_qual);
            //and the data size in the spectral domain
            final double halo_res_element_data = halo_res_element / source_flux.getSampling();
            //use the int value of spectral_pix as a smoothing element
            smoothing_element = new Double(halo_res_element_data + 0.5).intValue();
            if (smoothing_element < 1) smoothing_element = 1;
            ///////////////////////////////////////////////////////////////////////////////////////
            //  We Don't know why but using just the smoothing element is not enough to create the resolution
            //     that we expect.  Using a smoothing element of  =smoothing_element +1
            //     May need to take this out in the future.
            ///////////////////////////////////////////////////////////////////////////////////////
            smoothing_element = smoothing_element + 1;

            halo_flux.smoothY(smoothing_element);

            SampledSpectrumVisitor halo_resample = new ResampleWithPaddingVisitor(
                    obs_wave_low, obs_wave_high - 1,
                    //source_flux.getStart(), source_flux.getEnd(),
                    pix_width, 0);
            halo_flux.accept(halo_resample);
        }


        // resample both sky and SED
        final SampledSpectrumVisitor source_resample = new ResampleWithPaddingVisitor(
                obs_wave_low, obs_wave_high - 1,
                //source_flux.getStart(), source_flux.getEnd(),
                pix_width, 0);
        final SampledSpectrumVisitor background_resample = new ResampleWithPaddingVisitor(
                obs_wave_low, obs_wave_high - 1,
                //source_flux.getStart(), source_flux.getEnd(),
                pix_width, 0);


        source_flux.accept(source_resample);
        background_flux.accept(background_resample);

    }

    /** Calculates single and final S2N. */
    private void calculateS2N(final Slit slit) {

        // shot noise on dark current flux in aperture
        final double darkNoise = dark_current * slit.lengthPixels() * spec_exp_time;

        // readout noise in aperture
        final double readNoise = read_noise * read_noise * slit.lengthPixels();

        // signal and background for given slit and throughput
        final VisitableSampledSpectrum signal     = signal(throughput, spec_halo_source_fraction);
        final VisitableSampledSpectrum background = background(slit);

        // -- calculate and assign s2n results

        // S2N for one exposure
        spec_exp_s2n = singleS2N(signal, background, darkNoise, readNoise);

        // final S2N for all exposures
        spec_final_s2n = finalS2N(signal, background, darkNoise, readNoise);

    }

    /** Calculates signal and background. */
    private void calculateSignal(final Slit slit) {

        // total source flux in the aperture
        final VisitableSampledSpectrum signal         = signal(throughput, spec_halo_source_fraction);
        final VisitableSampledSpectrum sqrtBackground = background(slit);

        // create the Sqrt(Background) sed for plotting
        for (int i = _firstCcdPixel; i <= lastCcdPixel(sqrtBackground.getLength()); ++i)
            sqrtBackground.setY(i, Math.sqrt(sqrtBackground.getY(i)));

        // -- assign results
        spec_signal              = signal;
        sqrt_spec_var_background = sqrtBackground;

    }

    /** Calculates total source flux (signal) in the aperture. */
    private VisitableSampledSpectrum signal(final double throughput, final double haloThroughput) {

        final VisitableSampledSpectrum signal = (VisitableSampledSpectrum) source_flux.clone();
        final int lastPixel = lastCcdPixel(signal.getLength());
        for (int i = _firstCcdPixel; i <= lastPixel; ++i) {
            if (haloIsUsed) {
                signal.setY(i, totalFlux(source_flux.getY(i), throughput) + totalFlux(halo_flux.getY(i), haloThroughput));
            } else {
                signal.setY(i, totalFlux(source_flux.getY(i), throughput));
            }
        }

        return signal;
    }

    /** Calculates the background in the aperture. */
    private VisitableSampledSpectrum background(final Slit slit) {

        final VisitableSampledSpectrum background = (VisitableSampledSpectrum) background_flux.clone();

        //Shot noise on background flux in aperture
        for (int i = _firstCcdPixel; i <= lastCcdPixel(background.getLength()); ++i) {
            background.setY(i,
                    background_flux.getY(i) *
                            slit.width() * slit.pixelSize() * slit.lengthPixels() * // TODO: use slit.area()
                            spec_exp_time * disperser.dispersion());
        }

        return background;
    }

    /** Calculates the signal to noise ratio for a single exposure. */
    private VisitableSampledSpectrum singleS2N(final VisitableSampledSpectrum signal, final VisitableSampledSpectrum background, final double darkNoise, final double readNoise) {

        // total noise in the aperture
        final VisitableSampledSpectrum noise = (VisitableSampledSpectrum) source_flux.clone();
        for (int i = _firstCcdPixel; i <= lastCcdPixel(noise.getLength()); ++i) {
            noise.setY(i, Math.sqrt(signal.getY(i) + background.getY(i) + darkNoise + readNoise));
        }

        // calculate signal to noise
        final VisitableSampledSpectrum singleS2N = (VisitableSampledSpectrum) source_flux.clone();
        for (int i = _firstCcdPixel; i <= lastCcdPixel(singleS2N.getLength()); ++i) {
            singleS2N.setY(i, signal.getY(i) / noise.getY(i));
        }

        return singleS2N;

    }

    /** Calculates the final signal to noise ratio for all exposures. */
    private VisitableSampledSpectrum finalS2N(final VisitableSampledSpectrum signal, final VisitableSampledSpectrum background, final double darkNoise, final double readNoise) {

        // sky aper is either the aperture or the number of fibres in the IFU case
        final double skyAper;
        final AnalysisMethod analysisMethod = odp.analysisMethod();
        if (analysisMethod instanceof ApertureMethod) {
            skyAper = ((ApertureMethod) analysisMethod).skyAperture();
        } else if (analysisMethod instanceof IfuMethod) {
            skyAper = ((IfuMethod) analysisMethod).skyFibres();
        } else {
            throw new Error();
        }

        // calculate the noise factor for the given skyAper
        final double noiseFactor = 1 + (1 / skyAper);

        // the number of exposures measuring the source flux is
        final double spec_number_source_exposures = spec_number_exposures * spec_frac_with_source;

        // noise in aperture
        final VisitableSampledSpectrum spec_sourceless_noise = (VisitableSampledSpectrum) source_flux.clone();
        int spec_sourceless_noise_last = lastCcdPixel(spec_sourceless_noise.getLength());
        for (int i = _firstCcdPixel; i <= spec_sourceless_noise_last; ++i) {
            spec_sourceless_noise.setY(i, Math.sqrt(background.getY(i) + darkNoise + readNoise));
        }

        final VisitableSampledSpectrum finalS2N = (VisitableSampledSpectrum) source_flux.clone();
        for (int i = _firstCcdPixel; i <= lastCcdPixel(finalS2N.getLength()); ++i)
            finalS2N.setY(i, Math.sqrt(spec_number_source_exposures) *
                    signal.getY(i) /
                    Math.sqrt(signal.getY(i) + noiseFactor *
                            spec_sourceless_noise.getY(i) *
                            spec_sourceless_noise.getY(i)));

        return finalS2N;
    }

    private double totalFlux(final double flux, final double throughput) {
        return flux * throughput * spec_exp_time * disperser.dispersion();
    }



    public void setSourceSpectrum(final VisitableSampledSpectrum sed) {
        source_flux = sed;
    }

    public void setHaloSpectrum(final VisitableSampledSpectrum sed) {
        halo_flux = sed;
        haloIsUsed = true;
    }

    public void setSpecHaloSourceFraction(final double spec_halo_source_fraction) {
        this.spec_halo_source_fraction = spec_halo_source_fraction;
    }

    public void setHaloImageQuality(final double uncorrected_im_qual) {
        this.uncorrected_im_qual = uncorrected_im_qual;
    }

    public void setBackgroundSpectrum(final VisitableSampledSpectrum sed) {
        background_flux = sed;
    }

    public void setCcdPixelRange(final int first, final int last) {
        _firstCcdPixel = first;
        _lastCcdPixel = last;
    }

    public void setDisperser(final Disperser disperser) {
        this.disperser = disperser;
    }

    public void setSpectralPixelWidth(final double pix_width) {
        this.pix_width = pix_width;
    }

    public void setStartWavelength(final double obs_wave_low) {
        this.obs_wave_low = obs_wave_low;
    }

    public void setEndWavelength(final double obs_wave_high) {
        this.obs_wave_high = obs_wave_high;
    }

    public VisitableSampledSpectrum getSignalSpectrum() {
        return spec_signal;
    }

    public VisitableSampledSpectrum getBackgroundSpectrum() {
        return sqrt_spec_var_background;
    }

    public VisitableSampledSpectrum getExpS2NSpectrum() {
        return spec_exp_s2n;
    }

    public VisitableSampledSpectrum getFinalS2NSpectrum() {
        return spec_final_s2n;
    }

}