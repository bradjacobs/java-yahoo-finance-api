package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JacksonJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

abstract public class YahooResponseConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();

    // ensure that use of JsonPath will parse like the JsonMapper
    private static final Configuration JSON_PATH_CONFIG =
            Configuration.builder()
                    .jsonProvider(new JacksonJsonProvider(mapper))
                    .mappingProvider(new JacksonMappingProvider(mapper))
                    .options(EnumSet.noneOf(Option.class))
                    .build();


    abstract public List<Map<String,Object>> convertToListOfMaps(String json);

    abstract public Map<String,Map<String,Object>> convertToMapOfMaps(String json);



    public <T> List<T> convertToListOfPojos(String json, Class<T> targetType)
    {
        List<Map<String, Object>> listOfMaps = convertToListOfMaps(json);
        return convertToListOfPojos(listOfMaps, targetType);
    }

    public <T> List<T> convertToListOfPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType)
    {
        validateTargetClass(targetType);

        if (listOfMaps.isEmpty()) {
            return Collections.emptyList();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(List.class, targetType);
        return mapper.convertValue(listOfMaps, javaType);
    }


    public <T> Map<String,T> convertToMapOfPojos(String json, Class<T> targetType)
    {
        Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);
        return convertToMapOfPojos(mapOfMaps, targetType);
    }

    public <T> Map<String,T> convertToMapOfPojos(Map<String, Map<String, Object>> mapOfMaps, Class<T> targetType)
    {
        validateTargetClass(targetType);

        if (mapOfMaps.isEmpty()) {
            return Collections.emptyMap();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(Map.class, String.class, targetType);
        return mapper.convertValue(mapOfMaps, javaType);
    }





    private <T> void validateTargetClass(Class<T> targetType)
    {
        // just null check (for now)
        if (targetType == null) {
            throw new IllegalArgumentException("Must provide a target class type.");
        }
    }



    // todo fix technically strategy would be probably better than inheritance
    protected List<Map<String,Object>> convertToListOfMapsFromPath(String json, String path)
    {
        DocumentContext jsonDoc = createJsonPathDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyList();
        }
        return jsonDoc.read(path);
    }
    // todo fix technically strategy would be probably better than inheritance
    protected Map<String, Map<String, Object>> convertToMapOfMapsFromPath(String json, String path)
    {
        DocumentContext jsonDoc = createJsonPathDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyMap();
        }
        return jsonDoc.read(path);
    }

    private DocumentContext createJsonPathDocContext(String json) {

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
            // do a (slightly) nicer exception on parse error.
            throw new IllegalArgumentException("Unable to parse json: " + e.getMessage(), e);
        }
    }

}
