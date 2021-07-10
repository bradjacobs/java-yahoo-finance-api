package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class YahooRequestBuilder
{
    private YahooRequestBuilder() {}

    private static final YahooRequestBuilder INSTANCE = new YahooRequestBuilder();


    public static YahooRequestBuilder api() {
        return INSTANCE;
    }

    public ChartBuilder chart() {
        return new ChartBuilder();
    }

    public SparkBuilder spark() {
        return new SparkBuilder();
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
