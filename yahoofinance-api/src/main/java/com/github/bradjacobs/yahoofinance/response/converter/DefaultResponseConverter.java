package com.github.bradjacobs.yahoofinance.response.converter;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class DefaultResponseConverter extends YahooResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.*.result[*]";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // see if payload has a array of 'results' that can return
        try {
            return convertToListOfMapsFromPath(json, DEFAULT_LIST_PATH);
        }
        catch (Exception e)
        {
            // if original list attempt failed, this create a map result and shove into single list
            Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);
            return new ArrayList<>(mapOfMaps.values());
        }
    }


    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
       return convertToMapOfMapsFromPath(json, "$");
    }
}
