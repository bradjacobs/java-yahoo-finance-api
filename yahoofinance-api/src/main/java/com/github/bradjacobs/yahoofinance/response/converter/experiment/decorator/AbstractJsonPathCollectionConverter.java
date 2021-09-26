package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JacksonJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

import java.util.EnumSet;

abstract public class AbstractJsonPathCollectionConverter
{
    protected static final JsonMapper mapper = JsonMapperSingleton.getInstance();

    // ensure that use of JsonPath will parse like the JsonMapper
    protected static final Configuration JSON_PATH_CONFIG =
            Configuration.builder()
                    .jsonProvider(new JacksonJsonProvider(mapper))
                    .mappingProvider(new JacksonMappingProvider(mapper))
                    .options(EnumSet.noneOf(Option.class))
                    .build();


    protected DocumentContext createJsonPathDocContext(String json) {

        if (json == null) {
            throw new IllegalArgumentException("Must provide json");
        }
        if (json.isEmpty()) {
            return null;
        }
        try {
            return JsonPath.using(JSON_PATH_CONFIG).parse(json);
        }
        catch (Exception e) {
            // make a (slightly) nicer exception on parse error.
            throw new IllegalArgumentException("Unable to parse json: " + e.getMessage(), e);
        }
    }
}
