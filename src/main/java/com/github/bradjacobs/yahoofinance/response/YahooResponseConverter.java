package com.github.bradjacobs.yahoofinance.response;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

abstract public class YahooResponseConverter
{
    public abstract List<Map<String,Object>> convertToListOfMaps(String json);

    public Map<String,Map<String,Object>> convertToMapOfMaps(String json)
    {
        List<Map<String, Object>> listResults = convertToListOfMaps(json);

        String primaryKeyName = getPrimaryMapKeyName();

        // todo  keys are sorted (for now)
        Map<String, Map<String, Object>> resultMap = new TreeMap<>();
        for (Map<String, Object> entryMap : listResults) {
            Object keyValue = entryMap.get(primaryKeyName);
            resultMap.put(String.valueOf(keyValue), entryMap);
        }

        return resultMap;
    }

    abstract protected String getPrimaryMapKeyName();
}
