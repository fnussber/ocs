// Copyright 2000 Association for Universities for Research in Astronomy, Inc.,
// Observatory Control System, Gemini Telescopes Project.
// See the file COPYRIGHT for complete details.
//
// $Id: ConicTarget.java 18053 2009-02-20 20:16:23Z swalker $
//
package edu.gemini.spModel.target.system;

import edu.gemini.spModel.target.system.CoordinateParam.Units;
import edu.gemini.spModel.target.system.CoordinateTypes.*;

/**
 * This class represents a coordinate position as a set of
 * orbital elements.  The orbital elements can be converted
 * to a celestial position at any given time.
 *
 * @author      Shane Walker
 * @author      Kim Gillies (Modified for SP)
 */
public final class ConicTarget extends NonSiderealTarget  {
    /**
     * Options for the system type.
     */
    public static final class SystemType extends TypeBase {
        public static int _count = 0;

        public static final SystemType JPL_MINOR_BODY =
                new SystemType("JPL minor body");
        public static final SystemType MPC_MINOR_PLANET =
                new SystemType("MPC minor planet");

        public static final SystemType[] TYPES = new SystemType[]{
            JPL_MINOR_BODY,
            MPC_MINOR_PLANET,
        };

        private SystemType(String name) {
            super(_count++, name);
        }
    }

    /**
     * The base name of this coordinate system.
     */
    private static final String SHORT_SYSTEM_NAME = "conicTarget";

    /**
     * Default system type.
     */
    private static final SystemType DEFAULT_SYSTEM_TYPE = SystemType.JPL_MINOR_BODY;

    private static final double DEFAULT_E = 0.0;

    private ANode _anode = new ANode();
    private AQ _aq = new AQ();
    private double _e = DEFAULT_E;
    private Inclination _inclination = new Inclination();
    private LM _lm = new LM();
    private N _n = new N();
    private Perihelion _perihelion = new Perihelion();
    private Epoch _epochOfPeri = new Epoch("2000", Units.YEARS);


    /**
     * Provides clone support.
     */
    public Object clone() {
        ConicTarget result = (ConicTarget) super.clone();

        if (_anode != null) result._anode = (ANode) _anode.clone();
        if (_aq != null) result._aq = (AQ) _aq.clone();
        // _e is a double (immutable)

        if (_inclination != null) {
            result._inclination = (Inclination) _inclination.clone();
        }
        if (_lm != null) result._lm = (LM) _lm.clone();
        if (_n != null) result._n = (N) _n.clone();
        if (_perihelion != null) {
            result._perihelion = (Perihelion) _perihelion.clone();
        }
        if (_epochOfPeri != null ) {
            result._epochOfPeri =(Epoch)_epochOfPeri.clone();
        }
        return result;
    }

    /**
     * Override equqls to return true if both instances are the same.
     */
    public boolean equals(Object obj) {
        if (obj == null) return false;
        if (obj == this) return true;
        if (!super.equals(obj)) return false;

        if (!(obj instanceof ConicTarget)) return false;

        ConicTarget sys = (ConicTarget) obj;
        if (!(_anode.equals(sys._anode))) return false;
        if (!(_aq.equals(sys._aq))) return false;
        if (_e != sys._e) return false;
        if (!(_inclination.equals(sys._inclination))) return false;
        if (!(_lm.equals(sys._lm))) return false;
        if (!(_n.equals(sys._n))) return false;
        if (!(_perihelion.equals(sys._perihelion))) return false;
        if (!(_epochOfPeri.equals(sys._epochOfPeri))) return false;
        //if all of the above fails, the objects are equal.
        return true;
    }

    /**
     * Provide a hashcode for this object.  The class <code>{@link
     * CoordinateParam}</code> implements hashCode.
     */
    public int hashCode() {
        long hc = super.hashCode() ^
                _anode.hashCode() ^ _aq.hashCode() ^ _inclination.hashCode() ^
                _lm.hashCode() ^ _n.hashCode() ^ _perihelion.hashCode() ^ _epochOfPeri.hashCode();
        return (int) hc ^ (int) (hc >> 32);
    }

    /**
     * Constructs a default ConicTarget instance with default properties.
     */
    public ConicTarget() {
        // This can't really fail.
        super(DEFAULT_SYSTEM_TYPE);
    }

    /**
     * Constructs with the specific conic system type and default
     * values.
     */
    public ConicTarget(SystemType systemOption)
            throws IllegalArgumentException {
        super(systemOption);
    }



    /**
     * Gets the "anode" parameter, the longitude of the ascending node.
     */
    public ANode getANode() {
        if (_anode == null) {
            _anode = new ANode();
        }
        return _anode;
    }


    /**
     * Sets the anode.  The value of the parameter is not
     * copied so future modification will have an effect upon the value
     * stored in this class.  The newValue can be null.
     */
    public void setANode(ANode newValue) {
        _anode = newValue;
    }


    /**
     * Gets (a copy of) the "a or q", the mean distance (a), or perihelion
     * distance (q).
     */
    public AQ getAQ() {
        if (_aq == null) {
            _aq = new AQ();
        }
        return _aq;
    }


    /**
     * Sets the "a or q".  The value of the parameter is not
     * copied so that future modification will have an effect upon the value
     * stored in this class.  The new value can be null;
     */
    public void setAQ(AQ newValue) {
        _aq = newValue;
    }


    /**
     * Gets the orbital eccentricity.
     */
    public double getE() {
        return _e;
    }


    /**
     * Sets the orbital eccentricity.
     */
    public void setE(double newValue) {
        _e = newValue;
    }



    /**
     * Gets the epoch of perihelion of this object.
     */
    public Epoch getEpochOfPeri() {
        if (_epochOfPeri == null) {
            _epochOfPeri = _createDefaultEpochOfPeri();
        }
        return _epochOfPeri;
    }



    /**
     * Returns the current epoch of perihelion, creating it if necessary.
     */
    private Epoch _createDefaultEpochOfPeri() {
        return new Epoch("2000", Units.YEARS);
    }




    /**
     * Sets the epoch of perihelion.  The value of the parameter is not
     * copied so that future modification will have an effect upon the value
     * stored in this class.
     */
    public void setEpochOfPeri(Epoch newValue) {
        _epochOfPeri = newValue;
    }


    /**
     * Gets the inclination of the orbit (i).
     */
    public Inclination getInclination() {
        if (_inclination == null) {
            _inclination = new Inclination();
        }
        return _inclination;
    }


    /**
     * Sets the inclination. The value of the parameter is not
     * copied so that future modification will have an effect upon the value
     * stored in this class.
     */
    public void setInclination(Inclination newValue) {
        _inclination = newValue;
    }


    /**
     * Gets the "l or m", longitude (L) or mean anomoly (M).
     */
    public LM getLM() {
        if (_lm == null) {
            _lm = new LM();
        }
        return _lm;
    }


    /**
     * Sets the "l or m".  The value of the parameter is not
     * copied so that future modification will have an effect upon the value
     * stored in this class.
     */
    public void setLM(LM newValue) {
        _lm = newValue;
    }


    /**
     * Gets the mean daily motion.
     */
    public N getN() {
        if (_n == null) {
            _n = new N();
        }
        return _n;
    }


    /**
     * Sets the mean daily motion.  The value of the not
     * parameter is copied so that future modification will have an effect
     * upon the value stored in this class.
     */
    public void setN(N newValue) {
        _n = newValue;
    }


    /**
     * Gets the mean perihelion.
     */
    public Perihelion getPerihelion() {
        if (_perihelion == null) {
            _perihelion = new Perihelion();
        }
        return _perihelion;
    }


    /**
     * Sets the perihelion.  The value of the parameter
     * is not copied so that future modification will have an effect
     * upon the value stored in this class.
     */
    public void setPerihelion(Perihelion newValue) {
        _perihelion = newValue;
    }


    /**
     * Return the short system name.
     */
    public String getShortSystemName() {
        return SHORT_SYSTEM_NAME;
    }

    /**
     * Gets a short description of the position.
     */
    public String getPosition() {
        // what should be returned here
        return "Orbital Elements";
    }


    /**
     * Gets the available options for this coordinate system.
     */
    public TypeBase[] getSystemOptions() {
        return SystemType.TYPES;
    }


}
