// This software is Copyright(c) 2010 Association of Universities for
// Research in Astronomy, Inc.  This software was prepared by the
// Association of Universities for Research in Astronomy, Inc. (AURA)
// acting as operator of the Gemini Observatory under a cooperative
// agreement with the National Science Foundation. This software may 
// only be used or copied as described in the license set out in the 
// file LICENSE.TXT included with the distribution package.
//
// $Id: Test.java,v 1.4 2003/11/21 14:31:02 shane Exp $
//
// In the default package
package edu.gemini.itc.acqcam;

import java.io.IOException;

import edu.gemini.itc.parameters.TeleParameters;
import edu.gemini.itc.parameters.SourceDefinitionParameters;
import edu.gemini.itc.parameters.ObservingConditionParameters;
import edu.gemini.itc.parameters.ObservationDetailsParameters;
import edu.gemini.itc.shared.Recipe;

/**
 * This class runs a calculation without needing servlets, etc.
 * Parameters are hard-coded and then the class is recompiled.
 */
public class Test {
    // Run from command line to perform a calculation with fixed
    // parameters above.
    public static void main(String[] argv) throws IOException {
        try {
            
            // Create fake parameters  remember .POINT_SOURCE
            SourceDefinitionParameters sdp =
                    new SourceDefinitionParameters(SourceDefinitionParameters.POINT_SOURCE,
                    "",
                    20.0,
                    SourceDefinitionParameters.MAG,
                    .35,
                    SourceDefinitionParameters.FILTER,
                    "V",
                    0.,
                    0.,
                    SourceDefinitionParameters.NON_STELLAR_LIB +
                    "/elliptical-galaxy.nm",
                    //OR
                    //"modelBlackBody",
                    
                    //OR
                    //"modelEmLine",
                    4805,
                    1.25,
                    150,
                    1e-18,
                    5e-16,
                    SourceDefinitionParameters.WATTS_FLUX,
                    SourceDefinitionParameters.WATTS,
                    -1);
            
            ObservationDetailsParameters odp =
                    new ObservationDetailsParameters(
                    ObservationDetailsParameters.IMAGING,
                    ObservationDetailsParameters.INTTIME,
                    2,
                    1800.,
                    1.0,
                    10.,
                    ObservationDetailsParameters.IMAGING,
                    ObservationDetailsParameters.AUTO_APER,
                    0.7,
                    3);
            
            ObservingConditionParameters ocp =
                    new ObservingConditionParameters(2, 2, 4, 3, 2.3);
            
            AcquisitionCamParameters acp =
                    new AcquisitionCamParameters("R",
                    AcquisitionCamParameters.NDA);
            
            TeleParameters tp =
                    new TeleParameters(TeleParameters.ALUMINIUM,
                    TeleParameters.UP,
                    TeleParameters.OIWFS);
            
            
            
            // Create AcqCamRecipe object with the fake parameters
            Recipe recipe = new AcqCamRecipe(sdp, odp, ocp, acp, tp);
            
            // Perform Calculation
            recipe.writeOutput();
        } catch (Exception e) {
            // Any error during calculation will throw an exception and land here.
            e.printStackTrace();  // prints to console
            // Must do more to formulate an error message for the servlet.
        }
    }
}