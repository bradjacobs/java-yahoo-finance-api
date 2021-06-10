package com.github.bradjacobs.yahoofinance.response;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QuoteSummaryResponseConverter extends YahooResponseConverter
{
    private static final String DEFAULT_MAP_PATH = "$.quoteSummary.result[0]";

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);
        if (mapOfMaps == null) {
            return null;
        }

        List<Map<String, Object>> resultList = new ArrayList<>();
        for (Map.Entry<String, Map<String, Object>> entryMap : mapOfMaps.entrySet())
        {
            String moduleName = entryMap.getKey();
            Map<String, Object> moduleDataMap = entryMap.getValue();

            // creating a new map _only_ to have the module name appear first
            Map<String, Object> resultEntryMap = new LinkedHashMap<>();
            resultEntryMap.put("module", moduleName);
            resultEntryMap.putAll(moduleDataMap);
            resultList.add(resultEntryMap);
        }
        return resultList;
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        // todo - this won't work if get an 'error response'
        return JsonPath.read(json, DEFAULT_MAP_PATH);
    }

    @Override
    protected String getPrimaryMapKeyName()
    {
        return "";
    }
}
