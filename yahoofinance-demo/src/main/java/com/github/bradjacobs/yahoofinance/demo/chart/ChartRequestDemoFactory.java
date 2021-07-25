package com.github.bradjacobs.yahoofinance.demo.chart;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

public class ChartRequestDemoFactory
{
    private ChartRequestDemoFactory() {}

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            case 2: return EPOCH;
            case 3: return LIMITED_FIELDS;
            case 4: return SMALL_INTERVAL;
            default: return SIMPLE;
        }
    }



    /**
     * Query all prices for AAPL in the last year (and include dividend/split info)
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .chart()
            .withTicker("AAPL")
            .withRange(Range.TWO_YEARS)
            .withInterval(Interval.ONE_DAY)
            .withDividends(true)
            .withSplits(true)
            .build();

    /**
     * Query all prices for MSFT  b/w dates 2021-01-01 and 2021-03-31 (gmt) using epoch time values.
     */
    private static final YahooFinanceRequest EPOCH =
        YahooRequestBuilder.api()
            .chart()
            .withTicker("MSFT")
            .setTimeRange(1609459200L, 1617235199L)
            .build();

    /**
     * a. Query for AAPL
     * b. b/w dates 2021-01-01 and 2021-03-31 (gmt)
     * c. and have response only return close & adjclose  (i.e. no values for open/high/low/volume)
     */
    private static final YahooFinanceRequest LIMITED_FIELDS =
        YahooRequestBuilder.api()
            .chart()
            .withTicker("MSFT")
            .setTimeRange("2021-01-01", "2021-03-31")
            .withIndicatorCloseAdjCloseOnly()
            .build();

    private static final YahooFinanceRequest SMALL_INTERVAL =
        YahooRequestBuilder.api()
            .chart()
            .withTicker("AAPL")
            .withRange(Range.FIVE_DAYS)
            .withInterval(Interval.FIVE_MIN)
            .build();


}
