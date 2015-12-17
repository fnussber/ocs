package edu.gemini.itc.web.servlets;

import edu.gemini.itc.shared.*;
import org.jfree.chart.ChartUtilities;
import scala.Option;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This servlet provides data files and charts for spectroscopy results that have previously
 * been calculated and are cached by this servlet.
 */
public final class FilesServlet extends HttpServlet {

    public static final String ParamType        = "type";
    public static final String ParamId          = "id";
    public static final String ParamName        = "filename";
    public static final String ParamChartIndex  = "chartIndex";
    public static final String ParamSeriesIndex = "seriesIndex";
    public static final String ParamLoLimit     = "loLimit";
    public static final String ParamHiLimit     = "hiLimit";

    public static final String TypeImg          = "img";
    public static final String TypeTxt          = "txt";

    private static final Logger Log = Logger.getLogger(FilesServlet.class.getName());

    // === Caching
    // We need to keep the results of ITC calculations in memory for a while in order to be able to serve
    // requests for images and data files (spectras) when accessing the ITC calculations through the web page.
    // (The original ITC used to write files to /tmp but this is slower than doing all of this in memory
    // and also can clog up the disk drive if the /tmp files linger around for too long.)

    public static class IdTimedOutException extends RuntimeException {}

    private static class LRU extends LinkedHashMap<UUID, ItcSpectroscopyResult> {
        private static final int CacheLimit = 300;
        @Override protected boolean removeEldestEntry(final Map.Entry<UUID, ItcSpectroscopyResult> eldest) {
            return size() > CacheLimit;
        }
    }

    /** Hash map that temporarily stores calculation results which will be needed for charts and data files. */
    private static final Map<UUID, ItcSpectroscopyResult> cachedResult = Collections.synchronizedMap(new LRU());

    /** Caches a spectroscopy result. Called by Printer classes when creating HTML output. */
    public static UUID cache(final ItcSpectroscopyResult result) {
        final UUID id = UUID.randomUUID();
        cachedResult.put(id, result);
        return id;
    }

    /** Retrieves a cached result from UUID string. */
    private static ItcSpectroscopyResult result(final String id) {
        final ItcSpectroscopyResult r = cachedResult.get(UUID.fromString(id));
        if (r == null) throw new IdTimedOutException();
        return r;
    }
    
    // === End of caching

    /**
     * Called by server when an image or a result data file is requested.
     */
    public void doGet(final HttpServletRequest request, final HttpServletResponse response) throws IOException {

        try {
            final String filename = request.getParameter(ParamName);
            final String type     = request.getParameter(ParamType);
            final String id       = request.getParameter(ParamId);
            final int chartIndex  = Integer.parseInt(request.getParameter(ParamChartIndex));
            final Optional<Integer> seriesIndex = Optional.ofNullable(request.getParameter(ParamSeriesIndex)).map(Integer::parseInt);

            switch (type) {

                case TypeTxt:
                    response.setContentType("text/plain");
                    response.getOutputStream().write(toFile(id, filename, chartIndex, seriesIndex).getBytes());
                    break;

                case TypeImg:
                    response.setContentType("image/png");
                    final PlottingDetails pd = toPlottingDetails(request);
                    ChartUtilities.writeBufferedImageAsPNG(response.getOutputStream(), toImage(id, filename, chartIndex, pd));
                    break;

                default:
                    throw new Error();
            }

        } catch (final IdTimedOutException e) {
            // if this message comes up a lot we might need to tweak the cache settings
            Log.log(Level.WARNING, "Session has timed out, the requested result is not available anymore");
            response.sendError(HttpServletResponse.SC_REQUEST_TIMEOUT);

        } catch (final IllegalArgumentException e) {
            Log.log(Level.WARNING, "The request is malformed " + e.getMessage(), e);
            response.sendError(HttpServletResponse.SC_BAD_REQUEST);

        } catch (final Exception e) {
            Log.log(Level.WARNING, e.getMessage(), e);
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private static PlottingDetails toPlottingDetails(final HttpServletRequest request) {
        final String loLimitStr = request.getParameter(ParamLoLimit);
        final String upLimitStr = request.getParameter(ParamHiLimit);
        if (loLimitStr != null && upLimitStr != null) {
            final double loLimit = Double.parseDouble(loLimitStr);
            final double upLimit = Double.parseDouble(upLimitStr);
            return new PlottingDetails(PlottingDetails.PlotLimits.USER, loLimit, upLimit);
        } else {
            return PlottingDetails.Auto;
        }
    }

    private static BufferedImage toImage(final String id, final String filename, final int ix, final PlottingDetails pd) {
        final ItcSpectroscopyResult r = result(id);
        final ITCChart chart;
        switch (filename) {
            case "SignalChart":       chart = toChart(r, SignalChart.instance(),      ix, pd); break;
            case "S2NChart":          chart = toChart(r, S2NChart.instance(),         ix, pd); break;
            case "SignalPixelChart":  chart = toChart(r, SignalPixelChart.instance(), ix, pd); break;
            default:                  throw new Error("invalid chart type " + filename);
        }
        return chart.getBufferedImage(800, 600);
    }

    private static ITCChart toChart(final ItcSpectroscopyResult r, final SpcChartType chartType, final int ix, final PlottingDetails pd) {
        final Option<SpcChartData> chart = r.chart(chartType, ix);
        if (chart.isEmpty()) throw new Error("invalid chart type or chart index");
        return ITCChart.forSpcDataSet(chart.get(), pd);
    }

    // this is public because we use it for testing
    public static String toFile(final String id, final String filename, final int ix, final Optional<Integer> seriesIx) {
        final ItcSpectroscopyResult r = result(id);
        final String file;
        switch (filename) {
            case "SignalData":     file = toFile(r, SignalChart.instance(), SignalData.instance(),     ix, seriesIx); break;
            case "BackgroundData": file = toFile(r, SignalChart.instance(), BackgroundData.instance(), ix, seriesIx); break;
            case "SingleS2NData":  file = toFile(r, S2NChart.instance(),    SingleS2NData.instance(),  ix, seriesIx); break;
            case "FinalS2NData":   file = toFile(r, S2NChart.instance(),    FinalS2NData.instance(),   ix, seriesIx); break;
            default:               throw new Error("invalid data type " + filename);
        }
        return "# ITC Data: " + Calendar.getInstance().getTime() + "\n \n" + file;
    }

    private static String toFile(final ItcSpectroscopyResult r, final SpcChartType chartType, final SpcDataType dataType, final int ix, final Optional<Integer> seriesIx) {
        final Option<SpcChartData> chart = r.chart(chartType, ix);
        if (chart.isEmpty()) throw new Error("invalid chart type or chart index");
        return toFile(chart.get().allSeriesAsJava(dataType), seriesIx);
    }

    private static String toFile(final List<SpcSeriesData> dataSeries, final Optional<Integer> seriesIndex) {
        return seriesIndex.
                map(dataSeries::get).
                map(FilesServlet::toFile).
                orElse(toFiles(dataSeries));
    }

    private static String toFiles(final List<SpcSeriesData> dataSeries) {
        final StringBuilder sb = new StringBuilder();
        dataSeries.stream().
                map(FilesServlet::toFile).
                forEach(sb::append);
        return sb.toString();
    }

    private static String toFile(final SpcSeriesData data) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < data.data()[0].length; i++) {
            sb.append(String.format("%.3f\t%.3f\n", data.data()[0][i], data.data()[1][i]));
        }
        return sb.toString();
    }

}
