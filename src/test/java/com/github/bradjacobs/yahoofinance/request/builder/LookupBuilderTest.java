package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.LookupBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

public class LookupBuilderTest
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

        LookupBuilder builder =
            YahooRequestBuilder.api()
                .lookup()
                .withQuery(testQuery)
                .withCount(30)
                .withStart(10);


        YahooFinanceRequest req1 = builder.buildNext();
        YahooFinanceRequest req2 = builder.buildNext();
        YahooFinanceRequest req3 = builder.buildNext();

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