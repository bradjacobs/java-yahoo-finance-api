package com.github.bradjacobs.yahoofinance.demo.screener;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Exchange;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.types.Sector;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

/**
 */
public class ScreenerRequestDemoFactory
{
    private ScreenerRequestDemoFactory() {}


    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return BASECASE;
            case 2: return TOTAL_ONLY;
            case 3: return TOP_MARKET_CAP;
            default: return BASECASE;
        }
    }



    /**
     *  - region = US  (as in exchange, not company headquarters location)
     *  - netincome > 5%
     *  - currentratio > 2
     *  - in 'technology' sector
     *  - debt/equity < 200
     *  - eod price between $5 and $50
     */
    private static final YahooFinanceRequest BASECASE =
        YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .gt(ScreenerField.NETINCOME1YRGROWTH, 5)
            .gt(ScreenerField.CURRENTRATIO, 2)
            .in(ScreenerField.SECTOR, Sector.TECHNOLOGY)
            .lt(ScreenerField.TOTALDEBTEQUITY, 200)
            .btwn(ScreenerField.EODPRICE, 5, 50)
            .build();

    /**
     *  Get "total only" of Healthcare equities.
     */
    private static final YahooFinanceRequest TOTAL_ONLY =
        YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .in(ScreenerField.SECTOR, Sector.HEALTHCARE)
            .setTotalOnly(true)
            .build();

    /**
     *  Get 'top 10' results with the largest Market Cap in NASDAQ/NYSE
     *    (i.e.  sort by MarketCap descending + limit request to just 10)
     */
    private static final YahooFinanceRequest TOP_MARKET_CAP =
        YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .in(ScreenerField.EXCHANGE, Exchange.NASDAQ, Exchange.NASDAQGS, Exchange.NASDAQGM, Exchange.NASDAQCM, Exchange.NYSE)
            .setSize(10)
            .setSortDescending(ScreenerField.INTRADAYMARKETCAP)
            .build();

}
