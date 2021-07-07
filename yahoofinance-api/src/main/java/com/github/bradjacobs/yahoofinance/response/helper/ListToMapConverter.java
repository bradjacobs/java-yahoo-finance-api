package com.github.bradjacobs.yahoofinance.response.helper;

import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ListToMapConverter
{
    private ListToMapConverter() {}

    public static Map<String, Map<String,Object>> convertToMap(String keyName, List<Map<String, Object>> listData)
    {
        return convertToMap(keyName, listData, false);
    }

    public static Map<String, Map<String,Object>> convertToMap(String keyName, List<Map<String, Object>> listData, boolean sortMapKeys)
    {
        if (StringUtils.isEmpty(keyName)) {
            throw new IllegalArgumentException("Must provide a keyName");
        }
        if (listData == null) {
            return null;
        }
        else if (listData.size() == 0) {
            return Collections.emptyMap();
        }

        Map<String, Map<String,Object>> resultMap;
        if (sortMapKeys) {
            resultMap = new TreeMap<>();
        }
        else {
            resultMap = new LinkedHashMap<>();
        }

        for (Map<String, Object> entryMap : listData) {
            Object keyValue = entryMap.get(keyName);
            String keyValueString = (keyValue != null ? keyValue.toString() : "");
            if (keyValueString.length() == 0) {
                throw new IllegalArgumentException("One or more list entries is missing a value for key: " + keyName);
            }

            resultMap.put(keyValueString, entryMap);
        }

        return resultMap;
    }
}
