package com.github.bradjacobs.yahoofinance.response.converter;

import java.util.List;
import java.util.Map;

public class YahooResponseConverter implements ResponseConverter, ResponsePojoConverter
{
    private final ResponseConverter innerResponseConverter;
    private final ResponsePojoConverter innerResponsePojoConverter;

    public YahooResponseConverter(ResponseConverter innerResponseConverter, ResponsePojoConverter innerResponsePojoConverter)
    {
        this.innerResponseConverter = innerResponseConverter;
        this.innerResponsePojoConverter = innerResponsePojoConverter;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return innerResponseConverter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return innerResponseConverter.convertToMapOfMaps(json);
    }

    @Override
    public <T> List<T> convertToListOfPojos(String json, Class<T> targetType) {
        return innerResponsePojoConverter.convertToListOfPojos(json, targetType);
    }

    @Override
    public <T> List<T> convertToListOfPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType) {
        return innerResponsePojoConverter.convertToListOfPojos(listOfMaps, targetType);
    }

    @Override
    public <T> Map<String, T> convertToMapOfPojos(String json, Class<T> targetType) {
        return innerResponsePojoConverter.convertToMapOfPojos(json, targetType);
    }

    @Override
    public <T> Map<String, T> convertToMapOfPojos(Map<String, Map<String, Object>> mapOfMaps, Class<T> targetType) {
        return innerResponsePojoConverter.convertToMapOfPojos(mapOfMaps, targetType);
    }

}
