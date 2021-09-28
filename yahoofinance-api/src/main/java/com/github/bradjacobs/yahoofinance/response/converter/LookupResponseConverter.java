package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.helper.JsonFormatRemover;
import com.github.bradjacobs.yahoofinance.response.helper.ListToMapConverter;

import java.util.List;
import java.util.Map;

public class LookupResponseConverter extends YahooResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].documents[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // remove all of the 'raw', 'fmt' stuff (if exists)
        String updatedJson = JsonFormatRemover.removeFormats(json, false);

        // todo - this won't work if get an 'error response'
        return convertToListOfMapsFromPath(updatedJson, DEFAULT_LIST_PATH);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return ListToMapConverter.convertToMap(PRIMARY_MAP_KEY, convertToListOfMaps(json), true);
    }
}
