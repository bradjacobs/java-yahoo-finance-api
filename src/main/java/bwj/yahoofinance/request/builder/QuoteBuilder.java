package bwj.yahoofinance.request.builder;

import bwj.yahoofinance.types.YahooEndpoint;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class QuoteBuilder extends BaseRequestBuilder<QuoteBuilder>
{
    // use collection to allow for case where some endpoints allow multiple ticker values
    private Set<String> tickerSet = new LinkedHashSet<>();  // preserve insertion order

    public QuoteBuilder withTicker(String... tickers) {
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

    @Override
    protected YahooEndpoint _getRequestEndpoiint()
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
        return requestParamMap;
    }

    @Override
    protected QuoteBuilder getThis()
    {
        return this;
    }


    private String generateTickerString() {
        if (this.tickerSet.size() > 0) {
            return String.join(",", this.tickerSet);
        }
        return "";
    }
}
