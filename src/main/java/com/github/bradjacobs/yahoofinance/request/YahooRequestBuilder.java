package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.request.builder.EndpointBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.LookupBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.PriceHistoryBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.QuoteBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.QuoteSummaryBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.ScreenerBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.TimeSeriesBuilder;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class YahooRequestBuilder
{
    private YahooRequestBuilder() {}

    private static final YahooRequestBuilder INSTANCE = new YahooRequestBuilder();


    public static YahooRequestBuilder api() {
        return INSTANCE;
    }

    public PriceHistoryBuilder priceHistory() {
        return new PriceHistoryBuilder();
    }
    public QuoteSummaryBuilder quoteSummary() {
        return new QuoteSummaryBuilder();
    }

    public QuoteBuilder quote(){
        return new QuoteBuilder();
    }

    public TimeSeriesBuilder timeSeries(){
        return new TimeSeriesBuilder();
    }

    public ScreenerBuilder screener(){
        return new ScreenerBuilder();
    }

    public LookupBuilder lookup(){
        return new LookupBuilder();
    }

    // a generic builder for less common endponts (i.e. "everything else"
    public EndpointBuilder endpointRequest(YahooEndpoint endpoint){
        return new EndpointBuilder(endpoint);
    }

}
