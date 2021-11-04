package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JacksonJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

import java.util.EnumSet;

/**
 * VERY Simplistic Factory when you want a 'basic' JsonPath Configuration
 *   with default configs set.
 */
public class JsonPathConfigFactory {
    public static Configuration getConfig() {
        return builder().build();
    }

    public static Configuration getPrettyConfig() {
        return builder().usePretty(true).build();
    }

    public static JsonPathConfigFactory.Builder builder() {
        return new JsonPathConfigFactory.Builder();
    }


    public static class Builder {
        private boolean pretty = false;
        private boolean useBigDecimal = false;
        private JsonMapper customJsonMapper = null;

        public JsonPathConfigFactory.Builder usePretty(boolean pretty) {
            this.pretty = pretty;
            return this;
        }

        public JsonPathConfigFactory.Builder useBigDecimals(boolean useBigDecimal) {
            this.useBigDecimal = useBigDecimal;
            return this;
        }

        public JsonPathConfigFactory.Builder withMapper(JsonMapper jsonMapper) {
            this.customJsonMapper = jsonMapper;
            return this;
        }

        public Configuration build() {

            JsonMapper mapper = this.customJsonMapper;
            if (mapper == null) {
                mapper = JsonMapperFactory.builder().usePretty(pretty).useBigDecimals(useBigDecimal).build();
            }

            return Configuration.builder()
                    .jsonProvider(new JacksonJsonProvider(mapper))
                    .mappingProvider(new JacksonMappingProvider(mapper))
                    .options(EnumSet.noneOf(Option.class))
                    .build();
        }
    }
}


