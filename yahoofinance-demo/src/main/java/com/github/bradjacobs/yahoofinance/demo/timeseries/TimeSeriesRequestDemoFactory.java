package com.github.bradjacobs.yahoofinance.demo.timeseries;

import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import static com.github.bradjacobs.yahoofinance.types.YahooModule.ASSET_PROFILE;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.BALANCE_SHEET_HISTORY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.CASH_FLOW_STMT_HISTORY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.FINANCIAL_DATA;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.INCOME_STMT_HISTORY;

/**
 * TODO ***********  IMPORTANT NOTES:
 *    1. For non-premium members the data returned will be similar to that available on the website.
 *      (for example:  if you can only see the last 4 quarters of a income statement on the website, it will be true here as well)
 *  .
 *    2. It's possible that the list of fields in the request becomes "too big" and will result in a error.
 *       (i.e. it's not possible to specific "every possbile conceivable field value" for this reason)
 *  .
 *    3. Not all values are available for all tickers
 *      (for example: many tickers will return 'null' for GrossProfit, even though the company DID have a gross profit)
 */
public class TimeSeriesRequestDemoFactory
{
    private TimeSeriesRequestDemoFactory() {}

    private static final long ONE_YEAR_MILLIS = 31536000000L;

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            case 2: return ANNUAL;
            default: return SIMPLE;
        }
    }



    /**
     * Get all available timeseries data for AAPL for years 2019 and 2020
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .setStart(1546300800000L)
            .setEnd(1609459200000L)
            .build();


    /**
     * Get 'annual' timeseries INCOME_STATEMENT data for AAPL for the last couple years
     *
     * NOTE:  you may have to know when the annual reports are available for any given security
     */
    private static final YahooFinanceRequest ANNUAL =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withTimeFrame(TimeSeriesUnit.ANNUAL)
            .withStatement(StatementType.INC_STMT)
            .setStart("2017-12-31")
            .setEnd("2020-12-31")
            .build();

    // not yet available.
//    private static final YahooFinanceRequest SPECIFIC_FIELDS =
//        YahooRequestBuilder.api()
//            .timeSeries()
//            .withTicker("AAPL")
//            .withTimeFrame(TimeSeriesUnit.QUARTERLY)
//            .withStatement(StatementType.INC_STMT)


}
