package com.github.bradjacobs.yahoofinance.response.converter;


import com.github.bradjacobs.yahoofinance.response.converter.adapter.MapKeyFromListAdapter;

import java.util.List;
import java.util.Map;

public class ScreenerResponseConverter implements ResponseConverter
{
    private static final String PRIMARY_MAP_KEY = "symbol";
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].quotes";
    private final ResponseConverter converter;

    public ScreenerResponseConverter() {
        ResponseConverter inner = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new MapKeyFromListAdapter(inner, PRIMARY_MAP_KEY, true);
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return converter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return converter.convertToMapOfMaps(json);
    }
}
