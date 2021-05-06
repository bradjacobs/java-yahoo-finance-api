package bwj.yahoofinance.request;

import org.testng.annotations.Test;


import java.util.Arrays;

import static org.testng.Assert.*;

public class YahooScreenerRequestTest
{
    // todo: lots more tests.

    @Test
    public void testHappyPathCase() throws Exception
    {
        YahooScreenerRequest req = new YahooScreenerRequest.Builder()
            .setFormatted(true)
            .addFilterGt("ebitda", 999999)
            .addFilterLt("pe", 15)
            .addFilterInList("sector", Arrays.asList("industrials", "technology"))
            .build();

        assertNotNull(req);

    }

}
