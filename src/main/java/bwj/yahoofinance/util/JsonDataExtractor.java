/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.PrettyPrinter;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.node.MissingNode;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Map;

/**
 * Helper class to extract data from a JSON string blob
 *
 * todo: javadocs
 * todo: unittests
 * todo: better error/exception msg handling
 *
 * side note: possible perf issue if used under load.
 *   perf testing + refactoring can happen later.
 */
public class JsonDataExtractor
{
    private static final JsonMapper mapper = createDefaultObjectMapper();

    private final JsonNode rootNode;
    private boolean errorOnInvalidPath = true;


    public JsonDataExtractor(InputStream inputStream) {
        this( convertToText(inputStream) );
    }

    public JsonDataExtractor(Reader reader) {
        this( convertToText(reader) );
    }

    public JsonDataExtractor(String json)
    {
        if (StringUtils.isEmpty(json)) {
            throw new IllegalArgumentException("Must provide a JSON string");
        }

        try {
            rootNode = mapper.readTree(json);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON string: " + e.getMessage(), e);
        }
    }


    public String getInnerJson(String path) {
        return getInnerJson(path, false);
    }



    public String getInnerJson(String path, boolean pretty)
    {
        JsonNode node = getInnerNode(path);

        if (node.isMissingNode()) {
            return null;
        }

        try {
            if (pretty) {
                return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(node);
            }
            return node.toString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException(String.format("Unable to extract nested json from path '%s'. Error: %s", path, e.getMessage()), e);
        }
    }



    public List<String> getStrings(String path) {
        return getList(path, String.class);
    }
    public List<Long> getLongs(String path) {
        return getList(path, Long.class);
    }
    public List<Double> getDoubles(String path) {
        return getList(path, Double.class);
    }
    public List<Object> getObjects(String path) {
        return getList(path, Object.class);
    }
    public <T> List<T> getValues(String path, Class<T> clazz) {
        return getList(path, clazz);
    }




    protected <T> List<T> getList(String path, Class<T> clazz) {
        validateClassParameter(clazz);
        JsonNode innerNode = getInnerArrayNode(path);
        JavaType listOfClassType = getListClassType(clazz);
        return mapper.convertValue(innerNode, listOfClassType);
    }

    public String getString(String path) {
        return getValue(path, String.class);
    }
    public Long getLong(String path) {
        return getValue(path, Long.class);
    }
    public Double getDouble(String path) {
        return getValue(path, Double.class);
    }
    public Boolean getBoolean(String path) {
        return getValue(path, Boolean.class);
    }
    public Object getObject(String path) {
        return getValue(path, Object.class);
    }

    public <T> T getValue(String path, Class<T> clazz) {
        validateClassParameter(clazz);
        JsonNode innerNode = getInnerNode(path);
        return mapper.convertValue(innerNode, clazz);
    }




    public List<String> findStrings(String startingPath, String field) {
        return findValues(startingPath, field, String.class);
    }
    public List<Long> findLongs(String startingPath, String field) {
        return findValues(startingPath, field, Long.class);
    }
    public List<Double> findDoubles(String startingPath, String field) {
        return findValues(startingPath, field, Double.class);
    }
    public List<Object> findObjects(String startingPath, String field) {
        return findValues(startingPath, field, Object.class);
    }

    public <T> List<T> findValues(String startingPath, String field, Class<T> clazz) {
        validateFieldParameter(field);
        validateClassParameter(clazz);
        List<JsonNode> resultNodes = findNodeValues(startingPath, field);
        JavaType listOfClassType = getListClassType(clazz);
        return mapper.convertValue(resultNodes, listOfClassType);
    }

    protected List<JsonNode> findNodeValues(String startingPath, String field) {
        JsonNode innerNode = getInnerNode(startingPath);
        return innerNode.findValues(field);
    }


    protected void validateFieldParameter(String fieldName) throws IllegalArgumentException
    {
        // only blank is invalid
        if (StringUtils.isEmpty(fieldName)) {
            throw new IllegalArgumentException("Must provide a 'field' parameter.");
        }
    }

    /**
     * Validate if this a valid class.
     * @param clazz class to validate
     * @throws IllegalArgumentException if invalid class
     */
    protected <T> void validateClassParameter(Class<T> clazz) throws IllegalArgumentException
    {
        // for now, only a "NULL" is considered invalid.
        if (clazz == null) {
            throw new IllegalArgumentException("Must provide a 'class' parameter.");
        }
    }

    /**
     * Creates a JavaType definition for a "List of the given class"
     * @param clazz class type for definition
     * @return JavaType representing List of Class types
     */
    protected <T> JavaType getListClassType(Class<T> clazz) {
        return mapper.getTypeFactory().constructParametricType(List.class, clazz);
    }


    // "a few" special return types.
    public Map<String,Object> getMap(String path) {
        JsonNode innerNode = getInnerNode(path);
        return mapper.convertValue(innerNode, new TypeReference<Map<String, Object>>(){});
    }
    public Map<String,Map<String, Object>> getMapOfMaps(String path) {
        JsonNode innerNode = getInnerNode(path);
        return mapper.convertValue(innerNode, new TypeReference<Map<String, Map<String, Object>>>(){});
    }
    public List<Map<String, Object>> getListOfMaps(String path) {
        JsonNode innerNode = getInnerArrayNode(path);
        return mapper.convertValue(innerNode, new TypeReference<List<Map<String, Object>>>(){});
    }



    /**
     * Search for nested node at the given path, with the expectation that this represents and ARRAY.
     * @param path node path location
     * @return JsonNode
     */
    protected JsonNode getInnerArrayNode(String path)
    {
        JsonNode node = getInnerNode(path);
        if (!node.isArray()) {
            if (this.errorOnInvalidPath) {
                throw new IllegalArgumentException(String.format("Path does not point to a valid array: '%s'", path));
            }
            else {
                // if asked for array but it's not an array node, then mark as missing
                node = MissingNode.getInstance();
            }
        }
        return node;
    }


    /**
     * Search for nested node forthe give path.  A path must ALWAYS start with a slash "/"
     *   Use literal string "/" to represent the root node.
     *   For accessing an array element, include the array index as part of the path.
     * @param path path location of for the node to find relative to the root node.
     * @return JsonNode
     */
    protected JsonNode getInnerNode(String path) {

        if (StringUtils.isEmpty(path) || path.equals("/")) {
            return rootNode;
        }

        if (this.errorOnInvalidPath) {
            return rootNode.requiredAt(path);
        }
        else {
            return rootNode.at(path);
        }
    }


    public boolean isErrorOnInvalidPath()
    {
        return errorOnInvalidPath;
    }

    public void setErrorOnInvalidPath(boolean errorOnInvalidPath)
    {
        this.errorOnInvalidPath = errorOnInvalidPath;
    }




    public static String makeJson(Object obj) {
        return makeJson(obj, false);
    }

    public static String makeJson(Object obj, boolean pretty)
    {
        if (obj == null) {
            throw new IllegalArgumentException("must provide object(s) to serialize to JSON.");
        }

        try {
            if (pretty) {
                return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj);
            }
            return mapper.writeValueAsString(obj);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to convert to JSON: " + e.getMessage(), e);
        }
    }


    protected static String convertToText(InputStream inputStream) {
        if (inputStream == null) {
            throw new IllegalArgumentException("Must provide inputStream");
        }

        try {
            return IOUtils.toString(inputStream, StandardCharsets.UTF_8.name());
        }
        catch (IOException e) {
            throw new IllegalArgumentException("Unable to read InputStream: " + e.getMessage(), e);
        }
    }

    protected static String convertToText(Reader reader) {
        if (reader == null) {
            throw new IllegalArgumentException("Must provide reader");
        }

        try {
            return IOUtils.toString(reader);
        }
        catch (IOException e) {
            throw new IllegalArgumentException("Unable to read Reader: " + e.getMessage(), e);
        }
    }


    /**
     * Creates ObjectMapper with default custom settings
     * @return ObjectMapper
     */
    private static JsonMapper createDefaultObjectMapper()
    {
        SimpleModule module = new SimpleModule();
        module.addSerializer(Float.class, new DecimalSerializer<Float>());
        module.addSerializer(Double.class, new DecimalSerializer<Double>());
        module.addSerializer(BigDecimal.class, new DecimalSerializer<BigDecimal>());

        PrettyPrinter prettyPrinter = new DefaultPrettyPrinter()
            .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE);

        return JsonMapper.builder()
            .defaultPrettyPrinter(prettyPrinter)
            .addModule(module)
            .build();
    }

    /**
     * Custom Serializer for decimal number formats.
     *  (though "price" values are treated as a special case)
     * i.e.
     *   YES: "0.0002"
     *    NO: "2.0E-4"
     *    NO: "0.000200000000012"
     */
    private static class DecimalSerializer<T extends Number> extends StdSerializer<T>
    {
        private static final DecimalFormat formatter = new DecimalFormat();
        static {
            formatter.setMaximumFractionDigits(6); // max digits after decimal
            formatter.setMinimumFractionDigits(0);
            formatter.setGroupingUsed(false);
        }

        private static final String PRICE_FIELD = "price";

        public DecimalSerializer() {
            this(null);
        }

        public DecimalSerializer(Class<T> t) {
            super(t);
        }

        @Override
        public void serialize(T value, JsonGenerator gen, SerializerProvider provider) throws IOException {

            String serializedNumber;

            // allow special case for any "price" field to avoid output like: 32.700001  or  32.099998  ( a little kludgy )
            if (gen.getOutputContext().getCurrentName().toLowerCase().contains(PRICE_FIELD))
            {
                double numValue = value.doubleValue();
                if (value.doubleValue() < 1) {
                    serializedNumber = String.format("%.3f", numValue);
                }
                else {
                    serializedNumber = String.format("%.2f", numValue);
                }
            }
            else {
                serializedNumber = formatter.format(value);
            }

            gen.writeNumber( serializedNumber );
        }
    }

}
