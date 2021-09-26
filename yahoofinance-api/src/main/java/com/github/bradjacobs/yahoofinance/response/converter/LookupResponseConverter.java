package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.JsonNestedFormatRemoverDecorator;
import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.ListToMapKeyDecorator;
import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.JsonPathCollectionConverter;
import com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator.ResponseConverter;

import java.util.List;
import java.util.Map;

public class LookupResponseConverter extends YahooResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].documents[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    private final ResponseConverter targetConverter;

    public LookupResponseConverter()
    {
        ResponseConverter converter = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new JsonNestedFormatRemoverDecorator(converter, false);
        converter = new ListToMapKeyDecorator(converter, PRIMARY_MAP_KEY);
        this.targetConverter = converter;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // todo - this won't work if get an 'error response'
        return targetConverter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return targetConverter.convertToMapOfMaps(json);
    }
}
