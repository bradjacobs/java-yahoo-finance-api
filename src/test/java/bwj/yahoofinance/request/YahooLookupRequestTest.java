package bwj.yahoofinance.request;

import bwj.yahoofinance.YahooFinanceClient;
import org.testng.annotations.Test;

import java.util.Arrays;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

public class YahooLookupRequestTest
{
    // todo: lots more tests.

    /**
     * With buildNext, subsequent calls will include the start/offset value
     *   (thus used to get the next batch chunk)
     */
    @Test
    public void testBuildNext() throws Exception
    {
        String testQuery = "myQuery";
        YahooLookupRequest.Builder builder = new YahooLookupRequest.Builder()
            .withQuery(testQuery)
            .withCount(30)
            .withStart(10);

        YahooLookupRequest req1 = builder.buildNext();
        YahooLookupRequest req2 = builder.buildNext();
        YahooLookupRequest req3 = builder.buildNext();

        assertNotNull(req1);
        assertEquals(req1.getParam("query"), testQuery);
        assertEquals(req1.getParam("count"), "30");
        assertEquals(req1.getParam("start"), "10");

        assertNotNull(req2);
        assertEquals(req1.getParam("query"), testQuery);
        assertEquals(req2.getParam("count"), "30");
        assertEquals(req2.getParam("start"), "40");

        assertNotNull(req3);
        assertEquals(req1.getParam("query"), testQuery);
        assertEquals(req3.getParam("count"), "30");
        assertEquals(req3.getParam("start"), "70");
    }

}
