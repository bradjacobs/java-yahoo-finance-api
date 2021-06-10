package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.helper.ListToMapConverter;
import com.jayway.jsonpath.JsonPath;

import java.util.List;
import java.util.Map;


public class ScreenerResponseConverter implements YahooResponseConverter
{
    private static final String PRIMARY_MAP_KEY = "symbol";
    private static final String RESPONSE_ROOT_PATH = "$.finance.result[0].quotes";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        return JsonPath.read(json, RESPONSE_ROOT_PATH);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return ListToMapConverter.convertToMap(PRIMARY_MAP_KEY, convertToListOfMaps(json));
    }
}
