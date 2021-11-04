package com.github.bradjacobs.yahoofinance.util;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

public class JsonPathDocContextCreator
{
    private final Configuration jsonPathConfig;

    public JsonPathDocContextCreator() {
        this(null);
    }

    public JsonPathDocContextCreator(Configuration jsonPathConfig) {
        if (jsonPathConfig == null) {
            jsonPathConfig = JsonPathConfigFactory.getConfig();
        }
        this.jsonPathConfig = jsonPathConfig;
    }


    public DocumentContext createDocContext(String json) {

        if (json == null) {
            throw new IllegalArgumentException("Must provide json");
        }
        if (json.isEmpty()) {
            return null;
        }
        try {
            return JsonPath.using(jsonPathConfig).parse(json);
        }
        catch (Exception e) {
            // make a (slightly) nicer exception on parse error.
            throw new IllegalArgumentException("Unable to parse json: " + e.getMessage(), e);
        }
    }
}
