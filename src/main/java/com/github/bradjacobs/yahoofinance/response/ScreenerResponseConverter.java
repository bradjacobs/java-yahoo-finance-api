package com.github.bradjacobs.yahoofinance.response;

import com.jayway.jsonpath.JsonPath;

import java.util.List;
import java.util.Map;


public class ScreenerResponseConverter extends YahooResponseConverter
{
    private static final String PRIMARY_MAP_KEY = "symbol";
    private static final String RESPONSE_ROOT_PATH = "$.finance.result[0].quotes";

    @Override
    protected String getPrimaryMapKeyName()
    {
        return PRIMARY_MAP_KEY;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        return JsonPath.read(json,RESPONSE_ROOT_PATH);
    }
}
