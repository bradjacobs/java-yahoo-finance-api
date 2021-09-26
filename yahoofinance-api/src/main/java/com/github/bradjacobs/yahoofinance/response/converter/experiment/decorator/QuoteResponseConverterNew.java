package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;


public class QuoteResponseConverterNew extends AbstractWrappedResposneConverter
{
    private static final String DEFAULT_LIST_PATH = "$.quoteResponse.result[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    public QuoteResponseConverterNew() {
        super( generateNestedResponseConverter() );
    }

    private static ResponseConverter generateNestedResponseConverter() {
        ResponseConverter converter = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new ListToMapKeyDecorator(converter, PRIMARY_MAP_KEY, true);
        return converter;
    }
}
