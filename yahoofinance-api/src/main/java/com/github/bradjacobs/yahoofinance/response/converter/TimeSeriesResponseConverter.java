package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.timeseries.TimeSeriesValueMapGenerator;
import com.github.bradjacobs.yahoofinance.response.converter.util.JsonNestedFormatRemover;
import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;

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
    private static final String ELEMENT_NAMES_PATH = RESULT_OBJECTS_PATH + ".meta.type[0]";

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

        // first fetch all the names (aka names of the fields that were returned)
        String[] elementNames = jsonDoc.read(ELEMENT_NAMES_PATH, String[].class);

        List<Map<String,Object>> resultsDataList = jsonDoc.read(RESULT_OBJECTS_PATH);

        int entryCount = elementNames.length;
        for (int i = 0; i < entryCount; i++)
        {
            String elementName = elementNames[i];
            Map<String, Object> attributeDataMap = resultsDataList.get(i);
            if (attributeDataMap != null)
            {
                List<Map<String,Object>> elementDataList = (List<Map<String, Object>>) attributeDataMap.get(elementName);
                observer.updateAttributeMap(elementName, elementDataList);
            }
        }

        return observer.getMap();
    }
}