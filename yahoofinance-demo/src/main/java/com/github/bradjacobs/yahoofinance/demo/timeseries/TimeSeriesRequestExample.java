package com.github.bradjacobs.yahoofinance.demo.timeseries;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

public enum TimeSeriesRequestExample implements RequestExample
{
    SIMPLE {
        /**
         * Get all available timeseries data for AAPL for years 2019 and 2020
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.ANNUAL)
                    .setStart("2019-12-31")
                    .setEnd("2021-12-31")
                    .build();
        }
    },
    SIMPLE_ALT {
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .setStart(1546300800000L)
                    .setEnd(1609459200000L)
                    .build();
        }
    },
    ANNUAL {
        /**
         * Get 'annual' timeseries INCOME_STATEMENT data for AAPL for the last couple years
         *
         * NOTE:  you may have to know when the annual reports are available for any given security
         *   (e.g. must use a correct date range)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.ANNUAL)
                    .withStatement(StatementType.INC_STMT)
                    .setStart("2017-12-31")
                    .setEnd("2020-12-31")
                    .build();
        }
    },
    QUARTERLY {
        /**
         * Get 'quarterly' timeseries data for AAPL for the last couple years
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("CAT")
                    .withTimeFrame(TimeSeriesUnit.QUARTERLY)
                    .withStatement(StatementType.VALUE)
                    .setStart("2018-12-31")
                    .setEnd("2021-12-31")
                    .build();
        }
    },
    QTR_VALUE {
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.QUARTERLY)
                    .withStatement(StatementType.VALUE)
                    .setStart("2019-12-31")
                    .setEnd("2021-12-31")
                    .build();
        }
    },
    CUSTOM_FIELDS {
        /**
         * set explicit return fields.  Requires caller to "know" the exact field names  (no prefix required)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withCustomFields("quarterlyTotalRevenue", "quarterlyTotalDebt", "annualPeRatio")
                    .setStart(1546300800000L)
                    .setEnd(1609459200000L)
                    .build();
        }
    },
    CUSTOM_FIELDS_EXPLICIT {
        /**
         * set explicit return fields.  Requires caller to "know" the exact field names
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.QUARTERLY)
                    .withCustomFields("TotalRevenue", "TotalDebt", "PeRatio")
                    .setStart(1546300800000L)
                    .setEnd(1609459200000L)
                    .build();
        }
    },
    PREMIUM_REQUEST_1 {
        /**
         * Requires Authorization  (still in flux!!!!)
         */
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.QUARTERLY)
                    .withStatement(StatementType.INC_STMT)
                    .setPremium(true)
                    .setStart(1531268938L)
                    .setEnd(1625963338L)
                    .build();
        }
    };
}
