package com.github.bradjacobs.yahoofinance.response.converter.util;

import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;


/**
 * Using to construct a "map of maps" with a given listOfMaps plus the name of attribute to be used as the map key.
 */
public class SimpleMapOfMapsGenerator
{
    private final String keyName;
    private final boolean sortMapKeys;

    public SimpleMapOfMapsGenerator(String keyName, boolean sortMapKeys) {
        if (StringUtils.isEmpty(keyName)) {
            throw new IllegalArgumentException("Must provide a key name.");
        }

        this.keyName = keyName;
        this.sortMapKeys = sortMapKeys;
    }


    public Map<String, Map<String,Object>> convertToMap(List<Map<String, Object>> listData)
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
