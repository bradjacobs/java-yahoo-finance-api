package com.github.bradjacobs.yahoofinance.response.converter;


import com.github.bradjacobs.yahoofinance.response.converter.adapter.JsonNestedFormatRemoverAdapter;
import com.github.bradjacobs.yahoofinance.response.converter.adapter.MapKeyFromListAdapter;

public class LookupResponseConverter extends AbstractWrappedResposneConverter implements ResponseConverter
{
    private static final String DEFAULT_LIST_PATH = "$.finance.result[0].documents[*]";
    private static final String PRIMARY_MAP_KEY = "symbol";

    public LookupResponseConverter() {
        super(generateNestedResponseConverter());
    }

    private static ResponseConverter generateNestedResponseConverter() {
        ResponseConverter converter = new JsonPathCollectionConverter(DEFAULT_LIST_PATH, null);
        converter = new JsonNestedFormatRemoverAdapter(converter, false);
        converter = new MapKeyFromListAdapter(converter, PRIMARY_MAP_KEY, true);
        return converter;
    }
}
