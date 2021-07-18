package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.helper.ListToMapConverter;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QuoteResponseConverter extends YahooResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.quoteResponse.result[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // todo - this won't work if get an 'error response'
        return JsonPath.read(json, DEFAULT_LIST_PATH);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return ListToMapConverter.convertToMap(PRIMARY_MAP_KEY, convertToListOfMaps(json), true);
    }
}
