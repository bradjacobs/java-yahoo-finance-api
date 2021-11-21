package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.adapter.JsonNestedFormatRemoverAdapter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QuoteSummaryResponseConverter implements ResponseConverter
{
    private static final String DEFAULT_MAP_PATH = "$.quoteSummary.result[0]";
    private static final String MODULE_MAP_KEY_LABEL = "module";
    private final ResponseConverter converter;

    public QuoteSummaryResponseConverter() {
        ResponseConverter inner = new JsonPathCollectionConverter(null, DEFAULT_MAP_PATH);
        converter = new JsonNestedFormatRemoverAdapter(inner, false);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return converter.convertToMapOfMaps(json);
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        Map<String, Map<String, Object>> mapOfMaps = converter.convertToMapOfMaps(json);
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
            resultEntryMap.put(MODULE_MAP_KEY_LABEL, moduleName);
            resultEntryMap.putAll(moduleDataMap);
            resultList.add(resultEntryMap);
        }
        return resultList;
    }
}
