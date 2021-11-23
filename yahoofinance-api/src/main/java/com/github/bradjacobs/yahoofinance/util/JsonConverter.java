package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class JsonConverter
{
    private static final JsonMapper mapper = JsonMapperFactory.getMapper();
    private static final JsonMapper prettyMapper = JsonMapperFactory.getPrettyMapper();

    private JsonConverter() { }

    public static String toJson(Object obj) {
        if (obj == null) {
            return null;
        }
        try {
            return mapper.writeValueAsString(obj);
        }
        catch (JsonProcessingException e) {
            throw new InternalError("Unable to create json from object: " + e.getMessage(), e);
        }
    }

    public static String toPrettyJson(String json)
    {
        return toPrettyJson( convertToNode(json) );
    }

    public static String toPrettyJson(Object node)
    {
        try {
            return prettyMapper.writeValueAsString(node);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Unable to convert node to json string: " + e.getMessage(), e));
        }
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