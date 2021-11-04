package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class PrettyFormatter
{
    private static final JsonMapper prettyMapper = JsonMapperFactory.getPrettyMapper();

    private PrettyFormatter() { }

    // todo - see if this is any better or worse...
    //             String pretty = mapper.writeValueAsString(mapper.readValue(example, Object.class));

    public static String prettyJson(String json)
    {
        return prettyJson( convertToNode(json) );
    }

    public static String prettyJson(Object node)
    {
        try {
            return prettyMapper.writeValueAsString(node);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Unable to convert node to json string: " + e.getMessage(), e));
        }
    }

    private static JsonNode convertToNode(String json)
    {
        try {
            return prettyMapper.readTree(json);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON string: " + e.getMessage(), e);
        }
    }

}