/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.apache.commons.lang.StringUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class JsonUtil
{
    // todo: for now the mapper always does pretty formatting for debuggability/readability.
    //   this will probably not remain this way indefinitely.
    private static final ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);


    /**
     * Given a JSON structure, return JSON struction with the "raw" values flattened.
     * Example:
     *   BEFORE
     *     ...
     *     "cash": {
     *       "raw": 78016000000,
     *       "fmt": "78.02B",
     *       "longFmt": "78,016,000,000"
     *     },
     *   AFTER
     *     ...
     *     "cash": 38016000000
     * @param json jsonString
     * @return updated JSON  (NOTE: the returned JSON may have different indenting format)
     */
    public static String removeRawValues(String json)
    {
        Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);

        updateFlattenRawValues(mapOfMaps);

        return convertToJson(mapOfMaps);
    }


    public static String convertToJson(Object obj)
    {
        String jsonStr = null;
        try {
            jsonStr = mapper.writeValueAsString(obj);
        }
        catch (Exception e) {
            throw new RuntimeException("Unable to convert object to json: " + e.getMessage(), e);
        }
        return jsonStr;
    }

    /**
     * Recursive helper method for removing the "raw/fmt" submaps.
     * @param obj
     */
    private static void updateFlattenRawValues(Object obj)
    {
        if (obj instanceof List) {
            List<Object> objectList = (List<Object>) obj;

            for (Object listElement : objectList) {
                // continue recursion of each areay element.
                updateFlattenRawValues(listElement);
            }
        }
        else if (obj instanceof Map) {
            Map<String, Object> objectMap = (Map<String, Object>) obj;

            for (Map.Entry<String, Object> entry : objectMap.entrySet()) {
                Object objectMapValue = entry.getValue();

                // if the object is itself a map AND that map contains a 'raw'
                //   then take the row value and assign to the key (replacing the original map value for that key)
                // otherwise, just continue the recursion.
                Object rawValue = null;
                if (objectMapValue instanceof Map)
                {
                    Map<String, Object> subMap = (Map<String, Object>) objectMapValue;
                    rawValue = subMap.get("raw");
                }

                if (rawValue != null) {
                    entry.setValue(rawValue);
                }
                else {
                    // if no raw value found, then continue recursion as normal.
                    updateFlattenRawValues(objectMapValue);
                }
            }
        }
    }


    public static Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        if (StringUtils.isEmpty(json)) {
            return Collections.emptyMap();
        }

        try {
            return mapper.readValue(json, new TypeReference<Map<String, Map<String, Object>>>() {});
        }
        catch (JsonProcessingException e) {
            throw new RuntimeException("Unable to convert json string to map of maps: " + e.getMessage(), e);
        }
    }

}
