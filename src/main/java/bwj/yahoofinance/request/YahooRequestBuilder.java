package bwj.yahoofinance.request;

import bwj.yahoofinance.request.builder.EndpointBuilder;
import bwj.yahoofinance.request.builder.LookupBuilder;
import bwj.yahoofinance.request.builder.PriceHistoryBuilder;
import bwj.yahoofinance.request.builder.QuoteBuilder;
import bwj.yahoofinance.request.builder.QuoteSummaryBuilder;
import bwj.yahoofinance.request.builder.ScreenerBuilder;
import bwj.yahoofinance.request.builder.TimeSeriesBuilder;
import bwj.yahoofinance.types.YahooEndpoint;

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
