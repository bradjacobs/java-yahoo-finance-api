package com.github.bradjacobs.yahoofinance.demo.chart;

import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

import java.util.HashMap;
import java.util.Map;

public class ChartRequestDemoFactory
{
    private ChartRequestDemoFactory() {}




    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        return REQUEST_MAP.get(sampleRequestId);
    }





    /**
     * Query all prices for AAPL in the last year (and include divident/split info)
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .chart()
            .withTicker("AAPL")
            .withRange(Range.ONE_YEAR)
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




    private static final Map<Integer, YahooFinanceRequest> REQUEST_MAP = new HashMap<Integer, YahooFinanceRequest>(){{
        put(1, SIMPLE);
        put(2, EPOCH);
        put(3, LIMITED_FIELDS);
    }};


}
