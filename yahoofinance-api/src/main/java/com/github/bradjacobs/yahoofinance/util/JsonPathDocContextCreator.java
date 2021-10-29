package com.github.bradjacobs.yahoofinance.util;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

public class JsonPathDocContextCreator
{
    private final Configuration jsonPathConfig;

    // todo - come back w/ better soln for these constructors.
    public JsonPathDocContextCreator() {
        this(false, false);
    }

    public JsonPathDocContextCreator(boolean usePretty, boolean useBigDecimal) {
        this.jsonPathConfig = JsonPathConfigFactory.getConfig(usePretty, useBigDecimal);
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
