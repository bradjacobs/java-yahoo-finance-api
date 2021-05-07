/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.HashMap;
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
    private static final ObjectMapper mapper = new ObjectMapper();
    private static final ObjectMapper prettyMapper =
        new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .setDefaultPrettyPrinter(new DefaultPrettyPrinter().withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE));

    private static final boolean DEFAULT_EXCEPTION_ON_INVALID_PATH = true;


    private final JsonNode rootNode;
    private boolean exceptionOnInvalidPath;


    public JsonDataExtractor(String json) {
        this(json, DEFAULT_EXCEPTION_ON_INVALID_PATH);
    }


    public JsonDataExtractor(String json, boolean exceptionOnInvalidPath)
    {
        if (StringUtils.isEmpty(json)) {
            throw new IllegalArgumentException("must provide a json string");
        }

        try {
            rootNode = mapper.readTree(json);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON string: " + e.getMessage(), e);
        }
        this.exceptionOnInvalidPath = exceptionOnInvalidPath;
    }


    public String getInnerJson(String path) {
        return getInnerJson(path, false);
    }

    public String getPrettyInnerJson(String path) {
        return getInnerJson(path, true);
    }


    private String getInnerJson(String path, boolean pretty)
    {
        JsonNode node = getInnerNode(path);

        if (node.isMissingNode()) {
            return null;
        }

        try
        {
            if (pretty) {
                return prettyMapper.writeValueAsString(node);
            }
            else {
                return mapper.writeValueAsString(node);
            }
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Unable to extract nested json from path '%s'. Error: %s", path, e.getMessage()), e);
        }

    }




    public List<Object> findValues(String path, String field) {
        return findValues(path, field, Object.class);
    }
    public List<Long> findLongValues(String path, String field) {
        return findValues(path, field, Long.class);
    }
    public List<Double> findDoubleValues(String path, String field) {
        return findValues(path, field, Double.class);
    }
    public List<String> findStringValues(String path, String field) {
        return findValues(path, field, String.class);
    }


    public Object[] parseArray(String path) {
        JsonNode innerNode = getInnerArrayNode(path);
        return convert(Object[].class, innerNode);
    }
    public Long[] parseLongArray(String path) {
        JsonNode innerNode = getInnerArrayNode(path);
        return convert(Long[].class, innerNode);
    }
    public Double[] parseDoubleArray(String path) {
        JsonNode innerNode = getInnerArrayNode(path);
        return convert(Double[].class, innerNode);
    }
    public String[] parseStringArray(String path) {
        JsonNode innerNode = getInnerArrayNode(path);
        return convert(String[].class, innerNode);
    }

    public List<Object> parseList(String path) {
        return Arrays.asList(parseArray(path));
    }
    public List<Long> parseLongList(String path) {
        return Arrays.asList(parseLongArray(path));
    }
    public List<Double> parseDoubleList(String path) {
        return Arrays.asList(parseDoubleArray(path));
    }
    public List<String> parseStringList(String path) {
        return Arrays.asList(parseStringArray(path));
    }




    public Map<String,Object> parseMap(String path) {
        JsonNode innerNode = getInnerNode(path);

        try {
            return mapper.readValue(innerNode.toString(), new TypeReference<Map<String, Object>>() {});
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to convert node to map: " + e.getMessage(), e);
        }
    }

    public Map<String,Map<String, Object>> parseMapOfMaps(String path) {
        JsonNode innerNode = getInnerNode(path);

        try {
            return mapper.readValue(innerNode.toString(), new TypeReference<Map<String, Map<String, Object>>>() {});
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to convert node to map: " + e.getMessage(), e);
        }
    }

    public List<Map<String, Object>> parseListOfMaps(String path) {
        JsonNode innerNode = getInnerArrayNode(path);

        try {
            return mapper.readValue(innerNode.toString(), new TypeReference<List<Map<String, Object>>>() {});
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to convert node to map: " + e.getMessage(), e);
        }
    }



    protected <T> List<T> findValues(String path, String field, Class<T> clz) {
        List<JsonNode> resultNodes = findNodeValues(path, field);
        // convert the found nodes to array of given class type
        T[] results = (T[]) mapper.convertValue(resultNodes, classToArrayMap.get(clz));
        return Arrays.asList(results);
    }
    // note: might be more performant to use node.asLong() or node.asDouble(), etc
    //   depending on the given class type



    protected List<JsonNode> findNodeValues(String path, String field) {
        JsonNode innerNode = getInnerNode(path);
        return innerNode.findValues(field);
    }

    protected JsonNode getInnerArrayNode(String path)
    {
        JsonNode node = getInnerNode(path);
        if (!node.isArray() && this.exceptionOnInvalidPath) {
            throw new IllegalArgumentException(String.format("Path does not point to a valid array: '%s'", path));
        }
        return node;
    }

    protected JsonNode getInnerNode(String path) {

        if (StringUtils.isEmpty(path) || path.equals("/")) {
            return rootNode;
        }

        if (this.exceptionOnInvalidPath) {
            return rootNode.requiredAt(path);
        }
        else {
            return rootNode.at(path);
        }
    }



    protected <T> T convert(Class<T> clz, JsonNode node)
    {
        if (node.isMissingNode()) {
            return null;
        }

        return mapper.convertValue(node, clz);
    }




    public boolean isExceptionOnInvalidPath()
    {
        return exceptionOnInvalidPath;
    }

    public void setExceptionOnInvalidPath(boolean exceptionOnInvalidPath)
    {
        this.exceptionOnInvalidPath = exceptionOnInvalidPath;
    }



    // side note:
    //   the generic way to get array class is "Array.newInstance(clazz, 0).getClass()"
    //   but already know all the types going to encounter.
    private static final Map<Class,Class> classToArrayMap = new HashMap<Class,Class>() {{
        put(String.class, String[].class);
        put(Long.class, Long[].class);
        put(Double.class, Double[].class);
        put(Object.class, Object[].class);
    }};

}
