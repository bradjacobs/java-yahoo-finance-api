package com.github.bradjacobs.yahoofinance.demo.timeseries;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

/**
 * TODO ***********  IMPORTANT NOTES:
 *    1. For non-premium members the data returned will be similar to that available on the website.
 *      (for example:  if you can only see the last 4 quarters of a income statement on the website, it will be true here as well)
 *  .
 *    2. It's possible that the list of fields in the request becomes "too big" and will result in a error.
 *       (i.e. it's NOT possible to specify "every possbile conceivable field value" for this reason)
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
            case 1: return SIMPLE_A;
            case 2: return SIMPLE_B;
            case 3: return ANNUAL;
            case 4: return QUARTERLY;
            case 5: return MISC;
            case 6: return CUSTOM_FIELDS;
            case 7: return CUSTOM_FIELDS_EXPLICIT;
            case 8: return PREMIUM_REQUEST_1;
            default: return SIMPLE_A;
        }
    }


    /**
     * Get all available timeseries data for AAPL for years 2019 and 2020
     */

    private static final YahooFinanceRequest SIMPLE_A =
            YahooRequestBuilder.api()
                    .timeSeries()
                    .withTicker("AAPL")
                    .withTimeFrame(TimeSeriesUnit.ANNUAL)
                    .setStart("2019-12-31")
                    .setEnd("2021-12-31")
                    .build();


    private static final YahooFinanceRequest SIMPLE_B =
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



    /**
     * Get 'quarterly' timeseries data for AAPL for the last couple years
     *
     * NOTE:  you may have to know when the annual reports are available for any given security
     */
    private static final YahooFinanceRequest QUARTERLY =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("CAT")
            .withTimeFrame(TimeSeriesUnit.QUARTERLY)
            .withStatement(StatementType.VALUE)
            .setStart("2018-12-31")
            .setEnd("2021-12-31")
            .build();


    private static final YahooFinanceRequest MISC =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withTimeFrame(TimeSeriesUnit.QUARTERLY)
            .withStatement(StatementType.VALUE)
            .setStart("2019-12-31")
            .setEnd("2021-12-31")
            .build();

    // set explicity fields.  Requires caller to 'know' the exact names
    private static final YahooFinanceRequest CUSTOM_FIELDS =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withTimeFrame(TimeSeriesUnit.QUARTERLY)
            .withCustomFields("TotalRevenue", "TotalDebt", "PeRatio")
            .setStart(1546300800000L)
            .setEnd(1609459200000L)
            .build();


    private static final YahooFinanceRequest CUSTOM_FIELDS_EXPLICIT =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withCustomFields("quarterlyTotalRevenue", "quarterlyTotalDebt", "annualPeRatio")
            .setStart(1546300800000L)
            .setEnd(1609459200000L)
            .build();

    private static final YahooFinanceRequest PREMIUM_REQUEST_1  =
        YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withTimeFrame(TimeSeriesUnit.QUARTERLY)
            .withStatement(StatementType.INC_STMT)
            .setPremium(true)
            .setStart(1531268938L)
            .setEnd(1625963338L)
            .build();
}
