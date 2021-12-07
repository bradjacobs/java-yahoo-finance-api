package com.github.bradjacobs.yahoofinance.response.converter.timeseries;

import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TimeSeriesValueMapGenerator implements TimeSeriesResponseObserver
{
    private final List<TimeSeriesUnitObserver> subObserverList;

    // todo - fix - little kludgy (but low pri)
    public static TimeSeriesValueMapGenerator createDateKeyObserver() {
         return new TimeSeriesValueMapGenerator(Arrays.asList(
                new TimeSeriesDateKeyObserver(TimeSeriesUnit.ANNUAL),
                new TimeSeriesDateKeyObserver(TimeSeriesUnit.QUARTERLY),
                new TimeSeriesDateKeyObserver(TimeSeriesUnit.TRAILING)));
    }

    public static TimeSeriesValueMapGenerator createAttributeKeyObserver() {
        return new TimeSeriesValueMapGenerator(Arrays.asList(
                new TimeSeriesAttributeKeyObserver(TimeSeriesUnit.ANNUAL),
                new TimeSeriesAttributeKeyObserver(TimeSeriesUnit.QUARTERLY),
                new TimeSeriesAttributeKeyObserver(TimeSeriesUnit.TRAILING)));
    }

    private TimeSeriesValueMapGenerator(List<TimeSeriesUnitObserver> subObserverList) {
        this.subObserverList = subObserverList;
    }

    @Override
    public void updateAttributeMap(String elementName, List<Map<String,Object>> elementDataList)
    {
        for (TimeSeriesResponseObserver observer : subObserverList) {
            observer.updateAttributeMap(elementName, elementDataList);
        }
    }

    /**
     * Result map structure is different if only dealing with one type (say only 'quarterly') vs multiple types
     * @return
     */
    @Override
    public Map<String, Map<String, Object>> getMap() {

        Map<String,Map<String, Object>> metaMap = new LinkedHashMap<>();
        Map<String, Map<String, Object>> singleMap = null;

        for (TimeSeriesUnitObserver observer : subObserverList) {
            Map<String, Map<String, Object>> observerDataMap = observer.getMap();
            if (observerDataMap != null && observerDataMap.size() > 0) {
                singleMap = observerDataMap;
                metaMap.put( observer.getType().toString().toLowerCase(), createAltMapSignature(observerDataMap));
            }
        }

        if (metaMap.size() == 1) {
            return singleMap;
        }
        else {
            return metaMap;
        }
    }

    private Map<String, Object> createAltMapSignature(Map<String, Map<String, Object>> origDateResultMap)
    {
        return new LinkedHashMap<>(origDateResultMap);
    }
}
