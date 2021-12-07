package com.github.bradjacobs.yahoofinance.response.converter.timeseries;

import java.util.List;
import java.util.Map;

public interface TimeSeriesResponseObserver
{
    void updateAttributeMap(String elementName, List<Map<String,Object>> elementDataList);

    Map<String, Map<String,Object>> getMap();

}
