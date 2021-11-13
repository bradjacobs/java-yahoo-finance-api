package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.*;

public class EndpointRequestBuilder extends BaseRequestBuilder<EndpointRequestBuilder>
{
    private YahooEndpoint endpoint;

    // use collection to allow for case where some endpoints allow multiple ticker values
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();

    public EndpointRequestBuilder(YahooEndpoint endpoint)
    {
        this.endpoint = endpoint;
    }


    public EndpointRequestBuilder withTicker(String... tickers) {
        tickerSet.updateTickers(tickers);
        return this;
    }

    public EndpointRequestBuilder withEndpoint(YahooEndpoint endpoint) {
        this.endpoint = endpoint;
        return this;
    }

    @Override
    public YahooEndpoint getEndpoint()
    {
        return this.endpoint;
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

        if (this.endpoint != null)
        {
            if (this.endpoint.isMultiTickerSupported()) {
                requestParamMap.put(ParamKeys.SYMBOLS, generateTickerString());
            }
            else if (this.endpoint.isTickerKeyValueParam()) {
                requestParamMap.put(ParamKeys.SYMBOL, generateTickerString());
            }
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
