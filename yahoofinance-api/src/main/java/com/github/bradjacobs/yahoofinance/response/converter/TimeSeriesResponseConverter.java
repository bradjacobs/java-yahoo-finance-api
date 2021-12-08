package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.timeseries.TimeSeriesValueMapGenerator;
import com.github.bradjacobs.yahoofinance.response.converter.util.JsonNestedFormatRemover;
import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Converts the original timeseries response JSON is to a more "nice" format  (subjectively)
 */
/*
   todo items:
      - needs a lot of unit testing
      - might want to 'chop off' the day portion of the date so don't have "2021-06-25" "2021-06-26" as different map keys (though this "should" be rare)
      - MUCH code clean up required (was originally done just to see it work)
      - javadocs needed.
 */
public class TimeSeriesResponseConverter implements ResponseConverter
{
    private static final String RESULT_OBJECTS_PATH = "$.timeseries.result[*]";
    private static final String META_KEY = "meta";
    private static final String TIMESTAMP_KEY = "timestamp";

    private final JsonNestedFormatRemover jsonNestedFormatRemover = new JsonNestedFormatRemover(true);
    private final JsonPathDocContextCreator jsonPathDocContextCreator = new JsonPathDocContextCreator();

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        throw new UnsupportedOperationException("Returning data in List format is not supported for timeseries data.");
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        TimeSeriesValueMapGenerator observer = TimeSeriesValueMapGenerator.createDateKeyObserver();

        json = jsonNestedFormatRemover.removeFormats(json);
        DocumentContext jsonDoc = jsonPathDocContextCreator.createDocContext(json);

        // parse out sub list.  map within the list has 3 values 'meta', 'timestamp' and '__the_attribute_name__'
        List<Map<String,List<Map<String, Object>>>> resultsDataList = jsonDoc.read(RESULT_OBJECTS_PATH);

        // loop thru the list and make one master list.
        //  (the 'meta' and 'timestamp' keys will be overwritten each iteration, but don't care about those values.
        Map<String,List<Map<String, Object>>> buildMap = new LinkedHashMap<>();
        for (Map<String,List<Map<String, Object>>> stringObjectMap : resultsDataList) {
            buildMap.putAll(stringObjectMap);
        }

        // post cleanup.. remove the keys don't care about.
        buildMap.remove(META_KEY);
        buildMap.remove(TIMESTAMP_KEY);

        for (Map.Entry<String, List<Map<String, Object>>> entry : buildMap.entrySet()) {
            String elementName = entry.getKey();
            List<Map<String,Object>> elementDataList = entry.getValue();
            observer.updateAttributeMap(elementName, elementDataList);
        }

        return observer.getMap();
    }
}