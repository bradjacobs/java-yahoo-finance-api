package com.github.bradjacobs.yahoofinance.demo.quote;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

public enum QuoteRequestExample implements RequestExample
{
    SIMPLE {
        /**
         * Request Quote info for single ticker.
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quote()
                    .withTicker("AAPL")
                    .build();
        }
    },
    MULTI_TICKER {
        /**
         * Get all quote for multiple symbols
         *    *** NOTE: yahoo will have some upper bound so DO NOT try to pass in 20,000 symbols at once!!
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quote()
                    .withTicker("AAPL")
                    .withTicker("MSFT")
                    .withTicker("WBA")
                    .build();
        }
    },
    MULTI_TICKER_ALTERNATE {
        /**
         * Get all quote for multiple symbols
         *    *** NOTE: yahoo will have some upper bound so DO NOT try to pass in 20,000 symbols at once!!
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quote()
                    .withTicker("AAPL", "MSFT", "WBA")
                    .build();
        }
    };
}
