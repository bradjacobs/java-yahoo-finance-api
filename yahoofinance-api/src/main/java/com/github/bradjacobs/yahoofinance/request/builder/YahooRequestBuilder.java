package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class YahooRequestBuilder
{
    private YahooRequestBuilder() {}

    private static final YahooRequestBuilder INSTANCE = new YahooRequestBuilder();


    public static YahooRequestBuilder api() {
        return INSTANCE;
    }

    public ChartRequestBuilder chart() {
        return new ChartRequestBuilder();
    }

    public SparkRequestBuilder spark() {
        return new SparkRequestBuilder();
    }

    public QuoteSummaryRequestBuilder quoteSummary() {
        return new QuoteSummaryRequestBuilder();
    }

    public QuoteRequestBuilder quote(){
        return new QuoteRequestBuilder();
    }

    public TimeSeriesRequestBuilder timeSeries(){
        return new TimeSeriesRequestBuilder();
    }

    public ScreenerRequestBuilder screener(){
        return new ScreenerRequestBuilder();
    }

    public LookupRequestBuilder lookup(){
        return new LookupRequestBuilder();
    }

    public EarningsEventRequestBuilder earningsEvent(){
        return new EarningsEventRequestBuilder();
    }

    public IpoEventRequestBuilder ipoEvent(){
        return new IpoEventRequestBuilder();
    }

    // a generic builder for less common endpoints (i.e. "everything else"
    public EndpointRequestBuilder endpointRequest(YahooEndpoint endpoint){
        return new EndpointRequestBuilder(endpoint);
    }
}
