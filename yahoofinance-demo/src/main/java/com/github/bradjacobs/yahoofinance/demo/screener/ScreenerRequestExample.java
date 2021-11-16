package com.github.bradjacobs.yahoofinance.demo.screener;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Exchange;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.types.Sector;

public enum ScreenerRequestExample implements RequestExample
{
    BASECASE {
        /**
         *  - region = US  (as in exchange, not company headquarters location)
         *  - netincome > 5%
         *  - currentratio > 2
         *  - in 'technology' sector
         *  - debt/equity < 200
         *  - price between $5 and $50
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .screener()
                    .in(ScreenerField.REGION, Region.UNITED_STATES)
                    .gt(ScreenerField.NETINCOME1YRGROWTH, 5)
                    .gt(ScreenerField.CURRENTRATIO, 2)
                    .in(ScreenerField.SECTOR, Sector.TECHNOLOGY)
                    .lt(ScreenerField.TOTALDEBTEQUITY, 200)
                    .btwn(ScreenerField.INTRADAYPRICE, 5, 50)
                    .build();
        }
    },
    TOTAL_ONLY {
        /**
         *  Get "total only" of Healthcare equities.
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .screener()
                    .in(ScreenerField.REGION, Region.UNITED_STATES)
                    .in(ScreenerField.SECTOR, Sector.HEALTHCARE)
                    .setTotalOnly(true)
                    .build();
        }
    },
    TOP_MARKET_CAP {
        /**
         *  Get 'top 10' results with the largest Market Cap in NASDAQ/NYSE
         *    (i.e.  sort by MarketCap descending + limit request to just 10)
         */
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .screener()
                    .in(ScreenerField.REGION, Region.UNITED_STATES)
                    .in(ScreenerField.EXCHANGE, Exchange.NASDAQGS, Exchange.NASDAQGM, Exchange.NASDAQCM, Exchange.NYSE)
                    .setSize(10)
                    .setSortDescending(ScreenerField.INTRADAYMARKETCAP)
                    .build();
        }
    };
}
