package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class EndpointRequestBuilder extends BaseRequestBuilder<EndpointRequestBuilder>
{
    private final YahooEndpoint endpoint;

    // use collection to allow for case where some endpoints allow multiple ticker values
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();

    public EndpointRequestBuilder(YahooEndpoint endpoint)
    {
        if (endpoint == null) {
            throw new IllegalArgumentException("Request is missing endpoint value.");
        }
        this.endpoint = endpoint;
    }

    public EndpointRequestBuilder withTicker(Collection<String> tickers) {
        if (tickers != null) {
            withTicker(tickers.toArray(new String[0]));
        }
        return this;
    }

    public EndpointRequestBuilder withTicker(String... tickers) {
        tickerSet.updateTickers(tickers);
        return this;
    }

    @Override
    protected YahooEndpoint getEndpoint()
    {
        return this.endpoint;
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
        if (this.endpoint.isMultiTickerSupported()) {
            requestParamMap.put(ParamKeys.SYMBOLS, generateTickerString());
        }
        else if (this.endpoint.isTickerKeyValueParam()) {
            requestParamMap.put(ParamKeys.SYMBOL, generateTickerString());
        }
        return requestParamMap;
    }

    @Override
    protected EndpointRequestBuilder getThis()
    {
        return this;
    }

    private String generateTickerString() {
        return this.tickerSet.generateTickerString();
    }
}
