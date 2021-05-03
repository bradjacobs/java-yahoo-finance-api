package bwj.yahoofinance;

import bwj.yahoofinance.model.request.YahooFinanceRequest;
import org.testng.annotations.Test;
import static bwj.yahoofinance.YahooEndpoint.*;

public class YahooRequestValidatorTest
{
    private YahooRequestValidator validator = new YahooRequestValidator();

    // note: TestNG wouldn't let me do this.
    //private static final Class validationExceptionClass = IllegalArgumentException.class;


    private static final String EXPECTED_MISSING_REQUEST_MSG = "Request cannot be null.";
    private static final String EXPECTED_MISSING_TICKER_MSG = "Request is missing a valid ticker value.";
    private static final String EXPECTED_MISSING_ENDPOINT_MSG = "Request is missing endpoint value.";
    private static final String EXPECTED_BLANK_PARAM_KEY_MSG = "Cannot have a blank parameter key";
    private static final String EXPECTED_MISSING_MODULES_MSG = "QuoteSummary endpoint must have 1 or more modules value.";


    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_REQUEST_MSG)
    public void testMissingRquest() throws Exception {
        validator.validationRequest(null);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testMissingTicker() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest.Builder().withEndpoint(CHART).build();
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testBlankTicker() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest.Builder().withEndpoint(CHART).withTicker("").build();
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_ENDPOINT_MSG)
    public void testMissingEndpoint() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest.Builder().withTicker("AAPL").build();
        validator.validationRequest(req);
    }


    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_BLANK_PARAM_KEY_MSG)
    public void testBlankKeyParam() throws Exception {

        YahooFinanceRequest req = new YahooFinanceRequest.Builder()
                .withEndpoint(CHART)
                .withTicker("AAPL")
                .addParameter("", "some_value").build();
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_MODULES_MSG)
    public void testMissingModules() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest.Builder().withEndpoint(QUOTE_SUMMARY).withTicker("AAPL").build();
        validator.validationRequest(req);
    }

}
