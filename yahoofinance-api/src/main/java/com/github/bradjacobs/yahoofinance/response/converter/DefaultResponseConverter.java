package com.github.bradjacobs.yahoofinance.response.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DefaultResponseConverter extends AbstractWrappedResponseConverter implements ResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.*.result[*]";
    private static final String DEFAULT_MAP_PATH = "$";


    public DefaultResponseConverter() {
        super(generateNestedResponseConverter());
    }

    private static ResponseConverter generateNestedResponseConverter() {
        //   NOTE:   _might_ need to include removal of json nested formats  (tbd)
        return new JsonPathCollectionConverter(DEFAULT_LIST_PATH, DEFAULT_MAP_PATH);
    }


    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // see if payload has an array of 'results' that can return
        try {
            return super.convertToListOfMaps(json);
        }
        catch (Exception e)
        {
            // if original list attempt failed, then create a simple map result and shove into single list
            Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);
            return new ArrayList<>(mapOfMaps.values());
        }
    }

}
