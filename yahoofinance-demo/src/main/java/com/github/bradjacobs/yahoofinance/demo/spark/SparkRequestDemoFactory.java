package com.github.bradjacobs.yahoofinance.demo.spark;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.Type;

/**
 *
 */
public class SparkRequestDemoFactory
{
    private SparkRequestDemoFactory() {}

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            case 2: return MULTI;
            default: return SIMPLE;
        }
    }

    //  https://query1.finance.yahoo.com/v8/finance/spark?symbols=AAPL&range=5d&interval=1d&includePrePost=true

    /**
     * Get close prices for AAPL each day (last 5 days)
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .spark()
            .withTicker("AAPL")
            .withRange(Range.FIVE_DAYS)
            .withInterval(Interval.ONE_DAY)
            .build();


    private static final YahooFinanceRequest MULTI =
        YahooRequestBuilder.api()
            .spark()
            .withTicker("AAPL", "MSFT", "AMZN")
            .withRange(Range.FIVE_DAYS)
            .withInterval(Interval.ONE_DAY)
            .build();




}
