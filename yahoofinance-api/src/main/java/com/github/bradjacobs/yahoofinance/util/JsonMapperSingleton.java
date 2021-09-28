package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class JsonMapperSingleton
{
    private static final JsonMapper instance;
    private static final JsonMapper prettyInstance;

    static {
        instance = JsonMapper.builder()
            //.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS) // avoid big numbers as scientific notation
            //.enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN)   // avoid big numbers as scientific notation
            .enable(DeserializationFeature.USE_LONG_FOR_INTS)
            //.nodeFactory(JsonNodeFactory.withExactBigDecimals(true))  // todo - test what affect this line has.
            .build();

        prettyInstance = instance.rebuild()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .defaultPrettyPrinter(new DefaultPrettyPrinter()
                .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE))
            .build();
    }

    public static JsonMapper getInstance() {
        return instance;
    }

    public static JsonMapper getPrettyInstance() {
        return prettyInstance;
    }

}
