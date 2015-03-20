package edu.gemini.itc.operation;

public interface ImagingS2NCalculatable extends Calculatable {
    public void setSedIntegral(double sed_integral);

    public void setSecondaryIntegral(double secondary_integral);

    public void setSecondarySourceFraction(double secondary_source_fraction);

    public void setSkyIntegral(double sky_integral);

    public void setSkyAperture(double skyAper);

    public void setExtraLowFreqNoise(int elfnParam);

    public String getBackgroundLimitResult();

}
