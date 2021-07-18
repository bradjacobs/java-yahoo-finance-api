package com.github.bradjacobs.yahoofinance.response;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.util.Collections;
import java.util.List;
import java.util.Map;

abstract public class YahooResponseConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();


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


}
