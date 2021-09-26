package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;


public class LookupResponseConverterNew extends AbstractWrappedResposneConverter
{
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].documents[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    public LookupResponseConverterNew() {
        super(generateNestedResponseConverter());
    }

    private static ResponseConverter generateNestedResponseConverter() {
        ResponseConverter converter = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new JsonNestedFormatRemoverDecorator(converter, false);
        converter = new ListToMapKeyDecorator(converter, PRIMARY_MAP_KEY);
        return converter;
    }
}
