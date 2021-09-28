package com.github.bradjacobs.yahoofinance.response.converter;

import java.util.List;
import java.util.Map;

abstract public class AbstractWrappedResposneConverter implements ResponseConverter
{
    protected final ResponseConverter targetConveter;

    public AbstractWrappedResposneConverter(ResponseConverter targetConveter) {
        if (targetConveter == null) {
            throw new IllegalArgumentException("Must provide a target resposne convergter.");
        }
        this.targetConveter = targetConveter;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConveter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return targetConveter.convertToMapOfMaps(json);
    }
}
