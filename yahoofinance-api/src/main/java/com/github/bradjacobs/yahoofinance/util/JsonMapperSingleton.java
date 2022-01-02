package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import java.io.IOException;
import java.math.BigDecimal;

/**
 * VERY Simplistic Factory when you want a 'basic' jsonMapper
 *   with default configs set.
 */
//  SIDE:  ok, ok, it's technically not 'singleton' b/c there's 2 instances
//      (but not important enough to worry about at present)
public class JsonMapperSingleton
{
    private static final JsonMapper instance = createInstance(false);
    private static final JsonMapper prettyInstance = createInstance(true);

    public static JsonMapper getInstance() {
        return instance;
    }
    public static JsonMapper getPrettyInstance() {
        return prettyInstance;
    }

    private static JsonMapper createInstance(boolean makePretty)
    {
        SimpleModule module = new SimpleModule()
                .addSerializer(Float.class, new DecimalSerializer<>(Float.class))
                .addSerializer(Double.class, new DecimalSerializer<>(Double.class));

        JsonMapper.Builder builder = JsonMapper.builder()
                .enable(DeserializationFeature.USE_LONG_FOR_INTS)
                .enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN)
                .addModule(module);

        if (makePretty) {
            builder = builder
                    .enable(SerializationFeature.INDENT_OUTPUT)
                    .defaultPrettyPrinter(new DefaultPrettyPrinter()
                            .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));
        }
        return builder.build();
    }


    // Special number serializer to ensure NEVER WRITE SCIENTIFIC NOTATION !!
    private static class DecimalSerializer<T extends Number> extends StdSerializer<T> {
        public DecimalSerializer(Class<T> t) {
            super(t);
        }

        @Override
        public void serialize(T value, JsonGenerator gen, SerializerProvider provider) throws IOException {
            String numberAsString = value.toString();
            if (numberAsString.contains("E")) {
                numberAsString = BigDecimal.valueOf(value.doubleValue()).toPlainString();
            }
            gen.writeNumber( numberAsString );
        }
    }
}
