package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.util.Map;

import static com.github.bradjacobs.yahoofinance.types.YahooEndpoint.*;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

public class EndpointRequestBuilderTest
{
    private static final String EXPECTED_MISSING_ENDPOINT_MSG = "Request is missing endpoint value.";
    private static final String EXPECTED_MISSING_TICKER_MSG = "Request is missing a ticker value.";

    // todo - happy path tests

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_ENDPOINT_MSG)
    public void testMissingEndpoint() throws Exception {
        EndpointRequestBuilder endpointRequestBuilder = new EndpointRequestBuilder(null);
        YahooFinanceRequest request = endpointRequestBuilder.build();
    }
    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testMissingTickerParam() throws Exception {
        EndpointRequestBuilder endpointRequestBuilder = new EndpointRequestBuilder(ESG_CHART);
        YahooFinanceRequest request = endpointRequestBuilder.build();
    }

    // todo the 'blank' key parameter will just be ignored.
//    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_BLANK_PARAM_KEY_MSG)
//    public void testBlankParam() throws Exception {
//        EndpointRequestBuilder endpointRequestBuilder = new EndpointRequestBuilder(ESG_CHART);
//        YahooFinanceRequest request = endpointRequestBuilder
//                .withTicker("MSFT")
//                .addParam("", "some_value")
//                .build();
//
//    }

}
