package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.Map;

public class JsonConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();
    private static final JsonMapper prettyMapper = JsonMapperSingleton.getPrettyInstance();
    private static final TypeReference<Map<String, Object>> MAP_OF_OBJECTS_TYPE_REF = new TypeReference<Map<String, Object>>(){};

    private JsonConverter() { }

    public static String toJson(Object obj) {
        return toJson(obj, false);
    }

    public static String toPrettyJson(Object obj) {
        return toJson(obj, true);
    }

    private static String toJson(Object obj, boolean makePretty) {
        if (obj == null) {
            return null;
        }

        try {
            if (makePretty) {
                return prettyMapper.writeValueAsString(obj);
            }
            else {
                return mapper.writeValueAsString(obj);
            }
        }
        catch (JsonProcessingException e) {
            throw new InternalError("Unable to convert object to json string: " + e.getMessage(), e);
        }
    }

    public static Map<String,Object> toMapOfObjects(String json) {
        if (StringUtils.isEmpty(json)) {
            return Collections.emptyMap();
        }
        try {
            return mapper.readValue(json, MAP_OF_OBJECTS_TYPE_REF);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Unable to convert json string to map: " + e.getMessage(), e));
        }
    }

    public static String toPrettyJson(String json) {
        return toPrettyJson( convertToNode(json) );
    }


    public static JsonNode convertToNode(String json)
    {
        try {
            return prettyMapper.readTree(json);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON string: " + e.getMessage(), e);
        }
    }
}