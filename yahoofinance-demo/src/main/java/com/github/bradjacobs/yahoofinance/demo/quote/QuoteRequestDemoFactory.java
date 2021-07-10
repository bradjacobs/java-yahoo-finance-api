package com.github.bradjacobs.yahoofinance.demo.quote;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

/**
 */
public class QuoteRequestDemoFactory
{
    private QuoteRequestDemoFactory() {}

    private static final long ONE_YEAR_MILLIS = 31536000000L;

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            case 2: return MULTI_TICKER;
            case 3: return MULTI_TICKER_ALTERNATE;
            default: return SIMPLE;
        }
    }

    /**
     * Get 'annual' timeseries INCOME_STATEMENT data for AAPL for the last couple years
     *
     * NOTE:  you may have to know when the annual reports are available for any given security
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .quote()
            .withTicker("AAPL")
            .build();



    /**
     * Get all quote for multiple symbols
     *  *** NOTE: yahoo will have some upper bound so you don't try to pass in 20,000 symbols at once.
     */
    private static final YahooFinanceRequest MULTI_TICKER =
        YahooRequestBuilder.api()
            .quote()
            .withTicker("AAPL")
            .withTicker("MSFT")
            .withTicker("WBA")
            .build();

    /**
     * Get all quote for multiple symbols
     *  *** NOTE: yahoo will have some upper bound so you don't try to pass in 20,000 symbols at once.
     */
    private static final YahooFinanceRequest MULTI_TICKER_ALTERNATE =
        YahooRequestBuilder.api()
            .quote()
            .withTicker("AAPL", "MSFT", "WBA")
            .build();

}
