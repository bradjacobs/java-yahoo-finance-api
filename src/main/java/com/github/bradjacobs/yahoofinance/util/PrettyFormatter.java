package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

public class PrettyFormatter
{
    private static final ObjectMapper prettyMapper =
        new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .setDefaultPrettyPrinter(
                new DefaultPrettyPrinter()
                    .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));


    private PrettyFormatter() { }


    public static String prettyJson(String json)
    {
        return prettyJson( convertToNode(json) );
    }

    public static String prettyJson(JsonNode node)
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