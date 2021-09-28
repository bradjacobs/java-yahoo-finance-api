package com.github.bradjacobs.yahoofinance.response.converter;


import com.github.bradjacobs.yahoofinance.response.converter.adapter.MapKeyFromListAdapter;

public class ScreenerResponseConverter extends AbstractWrappedResponseConverter implements ResponseConverter
{
    private static final String PRIMARY_MAP_KEY = "symbol";
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].quotes";

    public ScreenerResponseConverter() {
        super(generateNestedResponseConverter());
    }

    private static ResponseConverter generateNestedResponseConverter() {
        ResponseConverter converter = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new MapKeyFromListAdapter(converter, PRIMARY_MAP_KEY, true);
        return converter;
    }

}
