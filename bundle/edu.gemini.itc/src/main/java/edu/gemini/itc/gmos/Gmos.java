package edu.gemini.itc.gmos;

import edu.gemini.itc.operation.DetectorsTransmissionVisitor;
import edu.gemini.itc.shared.GmosParameters;
import edu.gemini.itc.shared.IfuRadial;
import edu.gemini.itc.shared.IfuSingle;
import edu.gemini.itc.shared.ObservationDetails;
import edu.gemini.itc.shared.*;
import edu.gemini.spModel.gemini.gmos.GmosCommonType;
import edu.gemini.spModel.gemini.gmos.GmosNorthType;
import edu.gemini.spModel.gemini.gmos.GmosSouthType;

import java.awt.*;

/**
 * Gmos specification class
 */
public abstract class Gmos extends Instrument implements BinningProvider {

    //Plate scales for original and Hamamatsu CCD's (temporary)
    public static final double ORIG_PLATE_SCALE = 0.0727;
    public static final double HAM_PLATE_SCALE = 0.080778;

    protected DetectorsTransmissionVisitor _dtv;

    // Colors to use for charts corresponding to the detectorCcdIndex
    protected static final Color[] DETECTOR_CCD_COLORS = {Color.blue, Color.green, Color.red};

    /**
     * Related files will be in this subdir of lib
     */
    public static final String INSTR_DIR = "gmos";

    // Instrument reads its configuration from here.
    private static final double WELL_DEPTH = 125000.0;
    private static final double AD_SATURATION = 56636;
    private static final double HIGH_GAIN = 4.4;
    private static final double LOW_GAIN = 2.18;
    private static final int DETECTOR_PIXELS = 6218;

    // Used as a desperate solution when multiple detectors need to be handled differently (See REL-478).
    // For EEV holds the one instance one the Gmos instrument, for Hamamatsu, contains 3 one Gmos instance for
    // each of the three detectors.
    protected Gmos[] _instruments;

    protected final GmosParameters gp;
    protected final ObservationDetails odp;

    // Keep a reference to the color filter to ask for effective wavelength
    protected Filter _Filter;
    protected IFUComponent _IFU;
    protected GmosGratingOptics _gratingOptics;
    protected Detector _detector;
    protected double _sampling;

    // These are the limits of observable wavelength with this configuration.

    private int _detectorCcdIndex = 0; // 0, 1, or 2 when there are multiple CCDs in the detector

    public Gmos(final GmosParameters gp, final ObservationDetails odp, final String FILENAME, final int detectorCcdIndex) {
        super(INSTR_DIR, FILENAME);

        this.odp    = odp;
        this.gp     = gp;

        _detectorCcdIndex = detectorCcdIndex;

        _sampling = super.getSampling();

        // TODO: filter is not yet defined, need to work with filter from gp, clean this up
        if (!gp.filter().equals(GmosNorthType.FilterNorth.NONE) && !gp.filter().equals(GmosSouthType.FilterSouth.NONE)) {
            _Filter = Filter.fromWLFile(getPrefix(), gp.filter().name(), getDirectory() + "/");
            addFilter(_Filter);
        }


        FixedOptics _fixedOptics = new FixedOptics(getDirectory() + "/", getPrefix());
        addComponent(_fixedOptics);


        //Choose correct CCD QE curve
        switch (gp.ccdType()) {
            // E2V, site dependent
            case E2V:
                switch (gp.site()) {
                    // E2V for GN: gmos_n_E2V4290DDmulti3.dat      => EEV DD array
                    case GN:
                        _detector = new Detector(getDirectory() + "/", getPrefix(), "E2V4290DDmulti3", "EEV DD array");
                        _detector.setDetectorPixels(DETECTOR_PIXELS);
                        if (detectorCcdIndex == 0) _instruments = new Gmos[]{this};
                        break;
                    // E2V for GS: gmos_n_cdd_red.dat              => EEV legacy
                    case GS:
                        _detector = new Detector(getDirectory() + "/", getPrefix(), "ccd_red", "EEV legacy array");
                        _detector.setDetectorPixels(DETECTOR_PIXELS);
                        if (detectorCcdIndex == 0) _instruments = new Gmos[]{this};
                        break;
                    default:
                        throw new Error("invalid site");
                }
                break;
            // Hamamatsu, both sites: gmos_n_CCD-{R,G,B}.dat        =>  Hamamatsu (R,G,B)
            case HAMAMATSU:
                String fileName = getCcdFiles()[detectorCcdIndex];
                String name = getCcdNames()[detectorCcdIndex];
                Color color = DETECTOR_CCD_COLORS[detectorCcdIndex];
                _detector = new Detector(getDirectory() + "/", getPrefix(), fileName, "Hamamatsu array", name, color);
                _detector.setDetectorPixels(DETECTOR_PIXELS);
                if (detectorCcdIndex == 0)
                    _instruments = createCcdArray();
                break;
            default:
                throw new Error("invalid ccd type");
        }

        if (detectorCcdIndex == 0) {
            _dtv = new DetectorsTransmissionVisitor(gp.spectralBinning(),
                    getDirectory() + "/" + getPrefix() + "ccdpix_red" + Instrument.getSuffix());
        }

        if (isIfuUsed()) {
            if (gp.ifuMethod().get() instanceof IfuSingle) {
                _IFU = new IFUComponent(getPrefix(), ((IfuSingle) gp.ifuMethod().get()).offset());
            } else if (gp.ifuMethod().get() instanceof IfuRadial) {
                final IfuRadial ifu = (IfuRadial) gp.ifuMethod().get();
                _IFU = new IFUComponent(getPrefix(), ifu.minOffset(), ifu.maxOffset());
            } else {
                throw new Error("invalid IFU type");
            }
            addComponent(_IFU);
        }


        // TODO: grating is not yet defined, need to work with grating from gp, clean this up
        if (!gp.grating().equals(GmosNorthType.DisperserNorth.MIRROR) && !gp.grating().equals(GmosSouthType.DisperserSouth.MIRROR)) {
            _gratingOptics = new GmosGratingOptics(getDirectory() + "/" + getPrefix(), gp.grating(), _detector,
                    gp.centralWavelength(),
                    _detector.getDetectorPixels(),
                    gp.spectralBinning());
            _sampling = _gratingOptics.getGratingDispersion_nmppix();
            addGrating(_gratingOptics);
        }


        addComponent(_detector);


        // validate the current configuration
        validate();

    }

    /**
     * Returns an array containing this instrument, or, if there are multiple detector CCDs,
     * an array containing instances of this instrument with the CCD set differently
     * (Used to implement hamamatsu CCD support).
     */
    public Gmos[] getDetectorCcdInstruments() {
        return _instruments;
    }

    /**
     * Index of current CCD in detector
     *
     * @return 0, 1, or 2 when there are multiple CCDs in the detector (default: 0)
     */
    public int getDetectorCcdIndex() {
        return _detectorCcdIndex;
    }

    /**
     * Returns the name of the detector CCD
     */
    public String getDetectorCcdName() {
        return _detector.getName();
    }

    /**
     * Returns the color to use in plots of the detector CCD
     */
    public Color getDetectorCcdColor() {
        return _detector.getColor();
    }

    /**
     * Returns the effective observing wavelength.
     * This is properly calculated as a flux-weighted averate of
     * observed spectrum.  So this may be temporary.
     *
     * @return Effective wavelength in nm
     */
    public int getEffectiveWavelength() {
        if (grating.isEmpty()) return (int) _Filter.getEffectiveWavelength();
        else return (int) _gratingOptics.getEffectiveWavelength();

    }

    public double getGratingResolution() {
        return _gratingOptics.getGratingResolution();
    }

    public GmosCommonType.Disperser getGrating() {
        return gp.grating();
    }

    public double getGratingDispersion_nm() {
        return _gratingOptics.getGratingDispersion_nm();
    }

    public double getGratingDispersion_nmppix() {
        return _gratingOptics.getGratingDispersion_nmppix();
    }


    /**
     * Returns the subdirectory where this instrument's data files are.
     */
    public String getDirectory() {
        return ITCConstants.LIB + "/" + INSTR_DIR;
    }

    public double getPixelSize() {
        switch (gp.ccdType()) {
            case E2V:       return ORIG_PLATE_SCALE * gp.spatialBinning();
            case HAMAMATSU: return HAM_PLATE_SCALE * gp.spatialBinning();
            default:        throw new Error("invalid ccd type");
        }
    }

    public double getSpectralPixelWidth() {
        return _gratingOptics.getPixelWidth();
    }

    public double getWellDepth() {
        return WELL_DEPTH;
    }

    public double getSampling() {
        return _sampling;
    }

    public int getSpectralBinning() {
        return gp.spectralBinning();
    }

    public int getSpatialBinning() {
        return gp.spatialBinning();
    }

    public double getADSaturation() {
        return AD_SATURATION;
    }

    public double getHighGain() {
        return HIGH_GAIN;
    }

    public double getLowGain() {
        return LOW_GAIN;
    }

    public IFUComponent getIFU() {
        return _IFU;
    }

    public boolean isIfuUsed() {
        return gp.fpMask().isIFU();
    }

    //Abstract class for Detector Pixel Transmission  (i.e.  Create Detector gaps)
    public DetectorsTransmissionVisitor getDetectorTransmision() {
        return _dtv;
    }

    public String toString() {

        String s = "Instrument configuration: \n";
        s += super.opticalComponentsToString();

        if (!gp.fpMask().equals(GmosNorthType.FPUnitNorth.FPU_NONE) && !gp.fpMask().equals(GmosSouthType.FPUnitSouth.FPU_NONE))
            s += "<LI> Focal Plane Mask: " + gp.fpMask().displayValue() + "\n";
        s += "\n";
        if (odp.getMethod().isSpectroscopy())
            s += "<L1> Central Wavelength: " + gp.centralWavelength() + " nm" + "\n";
        s += "Spatial Binning: " + getSpatialBinning() + "\n";
        if (odp.getMethod().isSpectroscopy())
            s += "Spectral Binning: " + getSpectralBinning() + "\n";
        s += "Pixel Size in Spatial Direction: " + getPixelSize() + "arcsec\n";
        if (odp.getMethod().isSpectroscopy())
            s += "Pixel Size in Spectral Direction: " + getGratingDispersion_nmppix() + "nm\n";
        if (isIfuUsed()) {
            s += "IFU is selected,";
            if        (gp.ifuMethod().get() instanceof IfuSingle) {
                final IfuSingle ifu = (IfuSingle) gp.ifuMethod().get();
                s += "with a single IFU element at " + ifu.offset() + "arcsecs.";
            } else if (gp.ifuMethod().get() instanceof IfuRadial){
                final IfuRadial ifu = (IfuRadial) gp.ifuMethod().get();
                s += "with mulitple IFU elements arranged from " + ifu.minOffset() + " to " + ifu.maxOffset() + "arcsecs.";
            } else {
                throw new Error("invalid IFU type");
            }
            s += "\n";
        }
        return s;
    }

    protected abstract Gmos[] createCcdArray();
    protected abstract String getPrefix();
    protected abstract String[] getCcdFiles();
    protected abstract String[] getCcdNames();

    private void validate() {
        //Test to see that all conditions for Spectroscopy are met
        if (odp.getMethod().isSpectroscopy()) {
            if (grating.isEmpty())
                throw new RuntimeException("Spectroscopy calculation method is selected but a grating" +
                        " is not.\nPlease select a grating and a " +
                        "focal plane mask in the Instrument " +
                        "configuration section.");
            if (gp.fpMask().equals(GmosNorthType.FPUnitNorth.FPU_NONE) || gp.fpMask().equals(GmosSouthType.FPUnitSouth.FPU_NONE))
                throw new RuntimeException("Spectroscopy calculation method is selected but a focal" +
                        " plane mask is not.\nPlease select a " +
                        "grating and a " +
                        "focal plane mask in the Instrument " +
                        "configuration section.");
        }

        if (odp.getMethod().isImaging()) {
            if (filter.isEmpty())
                throw new RuntimeException("Imaging calculation method is selected but a filter" +
                        " is not.\n  Please select a filter and resubmit the " +
                        "form to continue.");
            if (grating.isDefined())
                throw new RuntimeException("Imaging calculation method is selected but a grating" +
                        " is also selected.\nPlease deselect the " +
                        "grating or change the method to spectroscopy.");
            if (!gp.fpMask().equals(GmosNorthType.FPUnitNorth.FPU_NONE) && !gp.fpMask().equals(GmosSouthType.FPUnitSouth.FPU_NONE))
                throw new RuntimeException("Imaging calculation method is selected but a Focal" +
                        " Plane Mask is also selected.\nPlease " +
                        "deselect the Focal Plane Mask" +
                        " or change the method to spectroscopy.");
            if (isIfuUsed())
                throw new RuntimeException("Imaging calculation method is selected but an IFU" +
                        " is also selected.\nPlease deselect the IFU or" +
                        " change the method to spectroscopy.");
        }

    }
}
