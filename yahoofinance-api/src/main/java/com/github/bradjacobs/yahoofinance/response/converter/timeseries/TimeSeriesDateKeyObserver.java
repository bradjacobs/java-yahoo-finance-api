package com.github.bradjacobs.yahoofinance.response.converter.timeseries;

import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

class TimeSeriesDateKeyObserver extends TimeSeriesUnitObserver
{
    private static final String EXTRA_DATE_STRING_LABEL = "date";

    private final Map<String, Map<String,Object>> valueMap = new TreeMap<>();

    public TimeSeriesDateKeyObserver(TimeSeriesUnit type) {
        super(type);
    }

    @Override
    protected void addMapEntry(String attributeName, String dateString, Number value) {
        Map<String, Object> attributeMap = valueMap.computeIfAbsent(dateString, k -> new TreeMap<>());
        attributeMap.put(attributeName, value);
    }

    @Override
    public Map<String, Map<String, Object>> getMap()
    {
        // it seems to be VERY useful to have the date in each nested map as well.
        //   so we'll go ahead and add
        if (valueMap.size() > 0) {
            List<String> dateKeys = new ArrayList<>(valueMap.keySet());
            for (String dateKey : dateKeys) {
                Map<String, Object> innerMap = valueMap.get(dateKey);
                innerMap.put(EXTRA_DATE_STRING_LABEL, dateKey);
            }
        }

        return valueMap;
    }
}
