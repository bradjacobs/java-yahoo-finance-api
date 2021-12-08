package com.github.bradjacobs.yahoofinance.demo.quotesummary;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.YahooModule;

import static com.github.bradjacobs.yahoofinance.types.YahooModule.*;

public enum QuoteSummaryRequestExample implements RequestExample
{
    SIMPLE {
        /**
         * Get the assetProfile Module for AAPL
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quoteSummary()
                    .withModules(ASSET_PROFILE)
                    .withTicker("AAPL")
                    .build();
        }
    },
    MULTI_MODULES {
        /**
         * Get multiple module financial data for MSFT
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quoteSummary()
                    .withModules(FINANCIAL_DATA)
                    .withModules(BALANCE_SHEET_HISTORY)
                    .withModules(INCOME_STMT_HISTORY)
                    .withModules(CASH_FLOW_STMT_HISTORY)
                    .withTicker("MSFT")
                    .build();
        }
    },
    MULTI_MODULES_ALTERNATIVE {
        /**
         * Get multiple module financial data for MSFT  (different form)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quoteSummary()
                    .withModules(FINANCIAL_DATA, BALANCE_SHEET_HISTORY, INCOME_STMT_HISTORY, CASH_FLOW_STMT_HISTORY)
                    .withTicker("MSFT")
                    .build();
        }
    },
    ALL_MODULES {
        /**
         * Grab _all_ information available to AAPL (all modules)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quoteSummary()
                    .withModules(YahooModule.values())
                    .withTicker("MSFT")
                    .build();
        }
    },
    BAD_TICKER {
        /**
         * Request with invalid (non-existent) ticker, will result in a 404 response.
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .quoteSummary()
                    .withModules(ASSET_PROFILE)
                    .withTicker("FAKEZZ")
                    .build();
        }
    };
}
