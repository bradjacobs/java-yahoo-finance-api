package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class DefaultResponsePojoConverter implements ResponsePojoConverter
{
    private static final JsonMapper mapper = JsonMapperFactory.getMapper();
    private final ResponseConverter targetConverter;

    public DefaultResponsePojoConverter(ResponseConverter targetConverter)
    {
        if (targetConverter == null) {
            throw new IllegalArgumentException("Must provider a target response converter.");
        }
        this.targetConverter = targetConverter;
    }


    @Override
    public <T> List<T> convertToListOfPojos(String json, Class<T> targetType)
    {
        List<Map<String, Object>> listOfMaps = targetConverter.convertToListOfMaps(json);
        return convertToListOfPojos(listOfMaps, targetType);
    }

    @Override
    public <T> List<T> convertToListOfPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType)
    {
        validateTargetClass(targetType);

        if (listOfMaps.isEmpty()) {
            return Collections.emptyList();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(List.class, targetType);
        return mapper.convertValue(listOfMaps, javaType);
    }

    @Override
    public <T> Map<String,T> convertToMapOfPojos(String json, Class<T> targetType)
    {
        Map<String, Map<String, Object>> mapOfMaps = targetConverter.convertToMapOfMaps(json);
        return convertToMapOfPojos(mapOfMaps, targetType);
    }

    @Override
    public <T> Map<String,T> convertToMapOfPojos(Map<String, Map<String, Object>> mapOfMaps, Class<T> targetType)
    {
        validateTargetClass(targetType);

        if (mapOfMaps.isEmpty()) {
            return Collections.emptyMap();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(Map.class, String.class, targetType);
        return mapper.convertValue(mapOfMaps, javaType);
    }

    protected <T> void validateTargetClass(Class<T> targetType)
    {
        // just null check (for now)
        if (targetType == null) {
            throw new IllegalArgumentException("Must provide a target class type.");
        }
    }

}
