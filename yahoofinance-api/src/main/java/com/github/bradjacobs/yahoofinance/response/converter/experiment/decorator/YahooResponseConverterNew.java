package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;

import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.ResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.ResponsePojoConverter;

import java.util.List;
import java.util.Map;

public class YahooResponseConverterNew implements ResponseConverter, ResponsePojoConverter
{
    private final ResponseConverter innerResposneConverter;
    private final ResponsePojoConverter innerResposnePojoConverter;

    public YahooResponseConverterNew(ResponseConverter innerResposneConverter, ResponsePojoConverter innerResposnePojoConverter)
    {
        this.innerResposneConverter = innerResposneConverter;
        this.innerResposnePojoConverter = innerResposnePojoConverter;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return innerResposneConverter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return innerResposneConverter.convertToMapOfMaps(json);
    }

    @Override
    public <T> List<T> convertToListOfPojos(String json, Class<T> targetType) {
        return innerResposnePojoConverter.convertToListOfPojos(json, targetType);
    }

    @Override
    public <T> List<T> convertToListOfPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType) {
        return innerResposnePojoConverter.convertToListOfPojos(listOfMaps, targetType);
    }

    @Override
    public <T> Map<String, T> convertToMapOfPojos(String json, Class<T> targetType) {
        return innerResposnePojoConverter.convertToMapOfPojos(json, targetType);
    }

    @Override
    public <T> Map<String, T> convertToMapOfPojos(Map<String, Map<String, Object>> mapOfMaps, Class<T> targetType) {
        return innerResposnePojoConverter.convertToMapOfPojos(mapOfMaps, targetType);
    }

}
