package com.github.bradjacobs.yahoofinance.demo.chart;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

public enum ChartRequestExample implements RequestExample
{
    SIMPLE {
        /**
         * Query all prices for AAPL in the last year (and include dividend/split info)
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .chart()
                    .withTicker("AAPL")
                    .withRange(Range.TWO_YEARS)
                    .withInterval(Interval.ONE_DAY)
                    .withDividends(true)
                    .withSplits(true)
                    .build();
        }
    },
    EPOCH {
        /**
         * Query all prices for MSFT  b/w dates 2021-01-01 and 2021-03-31 (gmt) using epoch time values.
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .chart()
                    .withTicker("MSFT")
                    .setTimeRange(1609459200L, 1617235199L)
                    .build();
        }
    },
    LIMITED_FIELDS {
        /**
         * a. Query for AAPL
         * b. b/w dates 2021-01-01 and 2021-03-31 (gmt)
         * c. and have response only return close & adjclose  (i.e. no values for open/high/low/volume)
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .chart()
                    .withTicker("MSFT")
                    .setTimeRange("2021-01-01", "2021-03-31")
                    .withIndicatorCloseAdjCloseOnly()
                    .build();
        }
    },
    SMALL_INTERVAL {
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .chart()
                    .withTicker("AAPL")
                    .withRange(Range.FIVE_DAYS)
                    .withInterval(Interval.FIVE_MIN)
                    .build();
        }
    };
}
