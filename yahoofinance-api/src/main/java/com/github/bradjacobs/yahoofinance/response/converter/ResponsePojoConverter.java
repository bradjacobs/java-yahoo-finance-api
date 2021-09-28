package com.github.bradjacobs.yahoofinance.response.converter;


import java.util.List;
import java.util.Map;

public interface ResponsePojoConverter
{
    <T> List<T> convertToListOfPojos(String json, Class<T> targetType);

    <T> List<T> convertToListOfPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType);

    <T> Map<String,T> convertToMapOfPojos(String json, Class<T> targetType);

    <T> Map<String,T> convertToMapOfPojos(Map<String, Map<String, Object>> mapOfMaps, Class<T> targetType);
}
