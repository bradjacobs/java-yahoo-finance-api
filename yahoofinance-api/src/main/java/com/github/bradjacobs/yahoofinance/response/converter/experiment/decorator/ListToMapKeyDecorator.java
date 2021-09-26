package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;

import org.apache.commons.lang3.StringUtils;

import java.util.*;

public class ListToMapKeyDecorator implements ResponseConverter
{
    private final ResponseConverter targetConveter;
    private final String keyName;
    private final boolean sortMapKeys;

    private static final boolean DEFAULT_SORT_MAP_KEYS = false;

    public ListToMapKeyDecorator(ResponseConverter targetResponseConveter, String keyName) {
        this(targetResponseConveter, keyName, DEFAULT_SORT_MAP_KEYS);
    }

    public ListToMapKeyDecorator(ResponseConverter targetResponseConveter, String keyName, boolean sortMapKeys) {
        if (targetResponseConveter == null) {
            throw new IllegalArgumentException("Must provide a target resposne converter.");
        }
        if (StringUtils.isEmpty(keyName)) {
            throw new IllegalArgumentException("Must provide a key name.");
        }

        this.targetConveter = targetResponseConveter;
        this.keyName = keyName;
        this.sortMapKeys = sortMapKeys;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConveter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {

        List<Map<String, Object>> listOfMaps = convertToListOfMaps(json);
        return convertToMap(listOfMaps);
    }


    protected Map<String, Map<String,Object>> convertToMap(List<Map<String, Object>> listData)
    {
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
