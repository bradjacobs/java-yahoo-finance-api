package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.*;

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


        // todo: right now only focusing on equities.  obviously this needs to be fixed when considering other types.
        List<String> fields = QuoteFieldFactory.getQuoteFields(Type.EQUITY);
        String fieldValueString = String.join(",", fields);
        requestParamMap.put(ParamKeys.FIELDS, fieldValueString);

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
