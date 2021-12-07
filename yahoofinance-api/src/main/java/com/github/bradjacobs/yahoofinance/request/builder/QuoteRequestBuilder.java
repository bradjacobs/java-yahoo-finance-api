package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QuoteRequestBuilder extends BaseRequestBuilder<QuoteRequestBuilder>
{
    // use collection to allow for case where some endpoints allow multiple ticker values
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();

    // todo - add batching logic if the ticker list is 'too big'
    public QuoteRequestBuilder withTicker(Collection<String> tickers) {
        if (tickers != null) {
            withTicker(tickers.toArray(new String[0]));
        }
        return this;
    }
    public QuoteRequestBuilder withTicker(String... tickers) {
        this.tickerSet.updateTickers(tickers);
        return this;
    }

    @Override
    protected YahooEndpoint getEndpoint()
    {
        return YahooEndpoint.QUOTE;
    }

    @Override
    protected String getRequestTicker()
    {
        return generateTickerString();
    }

    @Override
    protected Map<String, String> buildEndpointParamMap()
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
