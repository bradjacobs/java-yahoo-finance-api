package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;

/**
 * VERY Simplistic Factory when you want a 'basic' jsonMapper
 *   with default configs set.
 */
//  basically just keeping impl of creating a JsonMapper in a single location.
public class JsonMapperFactory
{
    public static JsonMapper getMapper() {
        return builder().build();
    }
    public static JsonMapper getPrettyMapper() {
        return builder().usePretty(true).build();
    }

    public static JsonMapperFactory.Builder builder() {
        return new JsonMapperFactory.Builder();
    }

    public static class Builder
    {
        private boolean pretty = false;
        private boolean useBigDecimal = false;

        public JsonMapperFactory.Builder usePretty(boolean pretty) {
            this.pretty = pretty;
            return this;
        }

        public JsonMapperFactory.Builder useBigDecimals(boolean useBigDecimal) {
            this.useBigDecimal = useBigDecimal;
            return this;
        }

        public JsonMapper build() {
            JsonMapper.Builder builder = JsonMapper.builder();
            if (useBigDecimal)
            {
                // mainly used for more precison over double and helps avoid
                //  'printing' values in scientific notation.
                builder = builder.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
                builder = builder.enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN);
                //builder = builder.nodeFactory(JsonNodeFactory.withExactBigDecimals(true)); // todo - test what affect this line has.
            }
            builder = builder.enable(DeserializationFeature.USE_LONG_FOR_INTS);

            if (pretty)
            {
                builder = builder.enable(SerializationFeature.INDENT_OUTPUT);
                builder = builder.defaultPrettyPrinter(new DefaultPrettyPrinter()
                        .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));
            }

            return builder.build();
        }
    }

}
