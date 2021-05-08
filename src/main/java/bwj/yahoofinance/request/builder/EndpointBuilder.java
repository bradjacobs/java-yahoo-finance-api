package bwj.yahoofinance.request.builder;

import bwj.yahoofinance.types.YahooEndpoint;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class EndpointBuilder extends BaseRequestBuilder<EndpointBuilder>
{
    private YahooEndpoint endpoint;

    // use collection to allow for case where some endpoints allow multiple ticker values
    private Set<String> tickerSet = new LinkedHashSet<>();  // preserve insertion order


    public EndpointBuilder(YahooEndpoint endpoint)
    {
        this.endpoint = endpoint;
    }


    public EndpointBuilder withTicker(String... tickers) {
        if (tickers != null && tickers.length > 0) {
            List<String> tickerList = Arrays.asList(tickers);
            tickerList.replaceAll(String::toUpperCase);  // make them all UPPERCASE
            this.tickerSet.addAll(tickerList);
        }
        else {
            tickerSet.clear();
        }
        return this;
    }

    public EndpointBuilder withEndpoint(YahooEndpoint endpoint) {
        this.endpoint = endpoint;
        return this;
    }



    @Override
    protected YahooEndpoint _getRequestEndpoiint()
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
    protected EndpointBuilder getThis()
    {
        return this;
    }


    private String generateTickerString() {
        if (this.tickerSet.size() > 0 && this.endpoint != null) {
            if (endpoint.isMultiTickerSupported()) {
                return String.join(",", this.tickerSet);
            }
            else {
                return this.tickerSet.iterator().next();
            }
        }
        return "";
    }

}
