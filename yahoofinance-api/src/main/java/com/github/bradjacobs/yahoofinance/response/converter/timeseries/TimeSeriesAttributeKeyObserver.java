package com.github.bradjacobs.yahoofinance.response.converter.timeseries;

import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import java.util.Map;
import java.util.TreeMap;

class TimeSeriesAttributeKeyObserver extends TimeSeriesUnitObserver
{
    private final Map<String, Map<String,Object>> valueMap = new TreeMap<>();

    public TimeSeriesAttributeKeyObserver(TimeSeriesUnit type) {
        super(type);
    }

    @Override
    protected void addMapEntry(String attributeName, String dateString, Number value)
    {
        Map<String, Object> dateMap = valueMap.computeIfAbsent(attributeName, k -> new TreeMap<>());
        dateMap.put(dateString, value);
    }

    @Override
    public Map<String, Map<String, Object>> getMap()
    {
        return valueMap;
    }
}
