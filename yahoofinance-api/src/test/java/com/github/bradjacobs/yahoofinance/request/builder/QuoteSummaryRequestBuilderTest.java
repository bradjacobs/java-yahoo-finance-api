package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import org.testng.annotations.Test;

import static com.github.bradjacobs.yahoofinance.types.YahooEndpoint.ESG_CHART;

public class QuoteSummaryRequestBuilderTest
{
    private static final String EXPECTED_MISSING_MODULES_MSG = "Endpoint QUOTE_SUMMARY is missing required parameter 'modules'.";

    // TODO -- fill in other tests!

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_MODULES_MSG)
    public void testMissingModules() throws Exception {

        YahooFinanceRequest req = YahooRequestBuilder.api()
                .quoteSummary()
                .withTicker("AAPL")
                .build();
    }
}
