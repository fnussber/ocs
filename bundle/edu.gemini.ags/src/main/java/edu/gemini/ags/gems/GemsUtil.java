package edu.gemini.ags.gems;

import edu.gemini.model.p1.immutable.SiderealTarget;
import edu.gemini.shared.skyobject.Magnitude;
import edu.gemini.shared.skyobject.SkyObject;
import edu.gemini.shared.util.immutable.Option;
import edu.gemini.spModel.core.Target;
import edu.gemini.spModel.target.SPTarget;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility methods for local use
 */
public class GemsUtil {

    /**
     * Sorts the target list, putting the brightest stars first and returns the sorted array.
     */
    public static SPTarget[] brightestFirstSPTarget(List<SPTarget> targetList) {
        SPTarget[] ar = targetList.toArray(new SPTarget[targetList.size()]);
        Arrays.sort(ar, new Comparator<SPTarget>() {
            @Override
            public int compare(SPTarget t1, SPTarget t2) {
                Option<Magnitude> m1 = t1.getMagnitude(Magnitude.Band.R);
                Option<Magnitude> m2 = t2.getMagnitude(Magnitude.Band.R);
                if (!m1.isEmpty() && !m2.isEmpty()) {
                    return m1.getValue().compareTo(m2.getValue());
                }
                return 0;
            }
        });
        return ar;
    }

    /**
     * Sorts the sky object list, putting the brightest stars first and returns the sorted array.
     */
    public static SkyObject[] brightestFirstSkyObject(List<SiderealTarget> flexureList) {
        SkyObject[] ar = flexureList.toArray(new SkyObject[flexureList.size()]);
        Arrays.sort(ar, new Comparator<SkyObject>() {
            @Override
            public int compare(SkyObject o1, SkyObject o2) {
                Option<Magnitude> m1 = o1.getMagnitude(Magnitude.Band.R);
                Option<Magnitude> m2 = o2.getMagnitude(Magnitude.Band.R);
                if (!m1.isEmpty() && !m2.isEmpty()) {
                    return m1.getValue().compareTo(m2.getValue());
                }
                return 0;
            }
        });
        return ar;
    }

    /**
     * Removes any duplicates from the list
     */
    public static List<Target.SiderealTarget> removeDuplicates(List<Target.SiderealTarget> list) {
        Map<String, Target.SiderealTarget> map = new HashMap<>(list.size());
        for(Target.SiderealTarget siderealTarget : list) {
            String name = siderealTarget.name();
            if (name != null) {
                map.put(name, siderealTarget);
            }
        }
        return new ArrayList<>(map.values());
    }

    /**
     * Returns a list of unique SkyObjects in the given search results.
     */
    public static List<Target.SiderealTarget> getUniqueSkyObjects(List<GemsCatalogSearchResults> list) {
        List<Target.SiderealTarget> result = new ArrayList<>();
        for(GemsCatalogSearchResults searchResults : list) {
            for(Target.SiderealTarget skyObject : searchResults.resultsAsJava()) {
                result.add(skyObject);
            }
        }
        return removeDuplicates(result);
    }
}
