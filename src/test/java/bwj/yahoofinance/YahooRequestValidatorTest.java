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
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setEndpoint(CHART);
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_TICKER_MSG)
    public void testBlankTicker() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setEndpoint(CHART);
        req.setTicker("");
        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_ENDPOINT_MSG)
    public void testMissingEndpoint() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        validator.validationRequest(req);
    }


    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_BLANK_PARAM_KEY_MSG)
    public void testBlankKeyParam() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        req.setEndpoint(CHART);
        req.addParam("", "some_value");

        validator.validationRequest(req);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class }, expectedExceptionsMessageRegExp = EXPECTED_MISSING_MODULES_MSG)
    public void testMissingModules() throws Exception {
        YahooFinanceRequest req = new YahooFinanceRequest();
        req.setTicker("AAPL");
        req.setEndpoint(QUOTE_SUMMARY);
        validator.validationRequest(req);
    }

}
