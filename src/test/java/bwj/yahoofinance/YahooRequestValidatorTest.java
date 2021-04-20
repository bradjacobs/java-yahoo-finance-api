package bwj.yahoofinance;

import org.testng.annotations.Test;
import static bwj.yahoofinance.YahooEndpoint.*;

public class YahooRequestValidatorTest
{
    private YahooRequestValidator validator = new YahooRequestValidator();

    // TestNG wouldn't let me do this.
    //private static final Class validationExceptionClass = IllegalArgumentException.class;


    private static final String EXPECTED_MISSING_REQUEST_MSG = "Request cannot be null.";
    private static final String EXPECTED_MISSING_TICKER_MSG = "Request is missing a valid ticker value.";
    private static final String EXPECTED_MISSING_ENDPOINT_MSG = "Request is missing endpoint value.";
    private static final String EXPECTED_INCOMPATIBLE_ENDPOINTS_MSG = "Multiple endpoints are only allowed for quote summary requests.";
    private static final String EXPECTED_BLANK_PARAM_KEY_MSG = "Cannot have a blank parameter key";


    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_REQUEST_MSG)
    public void testMissingRquest() throws Exception {
        validator.validationRequest(null);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testMissingTicker() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.addEndpoint(ASSET_PROFILE);
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testBlankTicker() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.addEndpoint(ASSET_PROFILE);
        req.setTicker("");
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_ENDPOINT_MSG)
    public void testMissingEndpoint() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_INCOMPATIBLE_ENDPOINTS_MSG)
    public void testIncompatibleEndpoints() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        req.addEndpoint(ASSET_PROFILE);
        req.addEndpoint(CHART);
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_BLANK_PARAM_KEY_MSG)
    public void testBlankKeyParam() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        req.addEndpoint(ASSET_PROFILE);
        req.addParam("", "some_value");

        validator.validationRequest(req);
    }


    // disabled for now.  The check for 2 more endpoints that are non-quotesummery will always throw before a version check (currently)
    @Test(enabled = false,
            expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_INCOMPATIBLE_ENDPOINTS_MSG)
    public void testMismatchingVersionEndpoints() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        req.addEndpoint(OPTIONS);
        req.addEndpoint(CHART);
        validator.validationRequest(req);
    }

}
