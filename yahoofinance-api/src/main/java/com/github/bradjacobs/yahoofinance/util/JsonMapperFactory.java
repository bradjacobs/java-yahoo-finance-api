package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.json.JsonWriteFeature;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.DecimalFormat;

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

            // TODO - ignoring 'useBigDecimal' for now... might just remove.

            SimpleModule module = new SimpleModule()
                .addSerializer(Float.class, new DecimalSerializer<>(Float.class))
                .addSerializer(Double.class, new DecimalSerializer<>(Double.class));

            JsonMapper.Builder builder = JsonMapper.builder()
                    .enable(DeserializationFeature.USE_LONG_FOR_INTS)
                    .enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN)
                    .addModule(module);

            if (pretty) {
                builder = builder
                        .enable(SerializationFeature.INDENT_OUTPUT)
                        .defaultPrettyPrinter(new DefaultPrettyPrinter()
                               .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));
            }

            return builder.build();
        }
    }

    // Special number serializer to ensure NEVER WRITE SCIENTIFIC NOTATION !
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
