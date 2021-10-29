package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;

import java.util.HashMap;
import java.util.Map;

public class JsonMapperFactory
{
    private static final Map<JsonConfigKey, JsonMapper> jsonMapperCache = new HashMap<>();

    public static JsonMapper getMapper() {
        return getMapper(JsonConfigKey.getDefaultConfigKey());
    }

    public static JsonMapper getMapper(boolean pretty, boolean useBigDecimal) {
        return getMapper(new JsonConfigKey(pretty, useBigDecimal));
    }

    protected static JsonMapper getMapper(JsonConfigKey jsonConfigKey)
    {
        JsonMapper mapper = jsonMapperCache.get(jsonConfigKey);
        if (mapper == null) {
            mapper = createNewMapper(jsonConfigKey);
        }
        return mapper;
    }


    private synchronized static JsonMapper createNewMapper(JsonConfigKey jsonConfigKey)
    {
        JsonMapper mapper = jsonMapperCache.get(jsonConfigKey);
        if (mapper != null) {
            return mapper;
        }

        JsonMapper.Builder builder = JsonMapper.builder();
        if (jsonConfigKey.isUseBigDecimal())
        {
            // mainly used for more precison over double and helps avoid
            //  'printing' values in scientific notation.
            builder = builder.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
            builder = builder.enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN);
            //builder = builder.nodeFactory(JsonNodeFactory.withExactBigDecimals(true)); // todo - test what affect this line has.
        }
        builder = builder.enable(DeserializationFeature.USE_LONG_FOR_INTS);

        if (jsonConfigKey.isMakePretty())
        {
            builder = builder.enable(SerializationFeature.INDENT_OUTPUT);
            builder = builder.defaultPrettyPrinter(new DefaultPrettyPrinter()
                        .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));
        }

        mapper = builder.build();
        jsonMapperCache.put(jsonConfigKey, mapper);
        return mapper;
    }

}
