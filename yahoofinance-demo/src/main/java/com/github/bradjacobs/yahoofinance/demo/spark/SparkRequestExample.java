package com.github.bradjacobs.yahoofinance.demo.spark;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

public enum SparkRequestExample implements RequestExample
{
    SIMPLE {
        /**
         * Get close prices for AAPL each day (last 5 days)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .spark()
                    .withTicker("AAPL")
                    .withRange(Range.FIVE_DAYS)
                    .withInterval(Interval.ONE_DAY)
                    .build();
        }
    },
    MULTI {
        /**
         * Get close prices for multiple tickers each day (last 5 days)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .spark()
                    .withTicker("AAPL", "MSFT", "AMZN")
                    .withRange(Range.FIVE_DAYS)
                    .withInterval(Interval.ONE_DAY)
                    .build();
        }
    },
    MULTI_SMALL_INTERVAL {
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .spark()
                    .withTicker("AAPL", "MSFT", "AMZN")
                    .withRange(Range.FIVE_DAYS)
                    .withInterval(Interval.FIFTEEN_MIN)
                    .build();
        }
    };
}
