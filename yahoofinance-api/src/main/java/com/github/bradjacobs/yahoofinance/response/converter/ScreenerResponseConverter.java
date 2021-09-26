package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.helper.ListToMapConverter;

import java.util.List;
import java.util.Map;


public class ScreenerResponseConverter extends YahooResponseConverter
{
    private static final String PRIMARY_MAP_KEY = "symbol";
    private static final String RESPONSE_ROOT_PATH = "$.finance.result[0].quotes";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        return convertToListOfMapsFromPath(json, RESPONSE_ROOT_PATH);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return ListToMapConverter.convertToMap(PRIMARY_MAP_KEY, convertToListOfMaps(json));
    }
}
