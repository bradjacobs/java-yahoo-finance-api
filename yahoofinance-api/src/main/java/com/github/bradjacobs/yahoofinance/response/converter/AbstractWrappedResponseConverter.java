package com.github.bradjacobs.yahoofinance.response.converter;

import java.util.List;
import java.util.Map;

abstract public class AbstractWrappedResponseConverter implements ResponseConverter
{
    protected final ResponseConverter targetConverter;

    public AbstractWrappedResponseConverter(ResponseConverter targetConverter) {
        if (targetConverter == null) {
            throw new IllegalArgumentException("Must provide a target response converter.");
        }
        this.targetConverter = targetConverter;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConverter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return targetConverter.convertToMapOfMaps(json);
    }
}
