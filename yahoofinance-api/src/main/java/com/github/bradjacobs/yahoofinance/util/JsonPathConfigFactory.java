package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JacksonJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

// todo - fix..  this is all clunky
public class JsonPathConfigFactory
{
    private static final Map<JsonConfigKey, Configuration> jsonConfigCache = new HashMap<>();

    public static Configuration getConfig() {
        return getConfig(JsonConfigKey.getDefaultConfigKey());
    }

    public static Configuration getConfig(boolean pretty, boolean useBigDecimal) {
        return getConfig(new JsonConfigKey(pretty, useBigDecimal));
    }

    private static Configuration getConfig(JsonConfigKey jsonConfigKey)
    {
        Configuration config = jsonConfigCache.get(jsonConfigKey);
        if (config == null) {
            config = createNewConfig(jsonConfigKey);
        }
        return config;
    }


    private synchronized static Configuration createNewConfig(JsonConfigKey jsonConfigKey)
    {
        Configuration config = jsonConfigCache.get(jsonConfigKey);
        if (config != null) {
            return config;
        }

        JsonMapper mapper = JsonMapperFactory.getMapper(jsonConfigKey);
        config = Configuration.builder()
                        .jsonProvider(new JacksonJsonProvider(mapper))
                        .mappingProvider(new JacksonMappingProvider(mapper))
                        .options(EnumSet.noneOf(Option.class))
                        .build();

        jsonConfigCache.put(jsonConfigKey, config);
        return config;
    }
}
