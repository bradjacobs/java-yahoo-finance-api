package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.*;

public class QuoteRequestBuilder extends BaseRequestBuilder<QuoteRequestBuilder>
{
    // use collection to allow for case where some endpoints allow multiple ticker values
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();

    public QuoteRequestBuilder withTicker(String... tickers) {
        this.tickerSet.updateTickers(tickers);
        return this;
    }

    @Override
    protected YahooEndpoint _getRequestEndpoint()
    {
        return YahooEndpoint.QUOTE;
    }

    @Override
    protected String _getRequestTicker()
    {
        return generateTickerString();
    }

    @Override
    protected Map<String, String> _buildParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();
        requestParamMap.put(ParamKeys.SYMBOLS, generateTickerString());


        // todo: right now only focusing on equities.  obviously this needs to be fixed when considering other types.
        List<String> fields = QuoteRequestFieldFactory.getQuoteFields(Type.EQUITY);
        String fieldValueString = String.join(",", fields);
        requestParamMap.put(ParamKeys.FIELDS, fieldValueString);

        return requestParamMap;
    }

    @Override
    protected QuoteRequestBuilder getThis()
    {
        return this;
    }

    private String generateTickerString() {
        return this.tickerSet.generateTickerString();
    }
}
