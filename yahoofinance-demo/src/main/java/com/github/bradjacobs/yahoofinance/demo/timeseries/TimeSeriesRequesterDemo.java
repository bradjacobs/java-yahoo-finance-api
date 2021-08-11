package com.github.bradjacobs.yahoofinance.demo.timeseries;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.util.Map;

public class TimeSeriesRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 2;

        YahooFinanceRequest req = TimeSeriesRequestDemoFactory.getRequest(exampleRequestId);

        timeSeriesRequestRunner(req);
    }


    private static void timeSeriesRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !(req.getEndpoint().equals(YahooEndpoint.TIMESERIES) || req.getEndpoint().equals(YahooEndpoint.PREMIUM_TIMESERIES))) {
            throw new IllegalArgumentException("Must supply a timeseries-type request");
        }

        String userName = null;
        String password = null;

        YahooFinanceClient client;

        if (StringUtils.isNotEmpty(userName) && StringUtils.isNotEmpty(password)) {
            client = new YahooFinanceClient(userName, password);
        }
        else {
            client = new YahooFinanceClient();
        }


        YahooResponse resp = client.execute(req);

        String rawJson = resp.getJson();
        String prettyJson = resp.getPrettyJson();

        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }


    //    NOTE..  one possible format for the mapOfMaps response.
    //
    // "annual" ..
    //     "2019-09-30"
    //          "accountsPayable": 46236000000
    //          "accountsReceivable": 22926000000
    //          "accumulatedDepreciation": -58579000000
    //          ....
    //     "2020-09-30"
    //          "accountsPayable": 42296000000
    //          "accountsReceivable": 16120000000
    //          "accumulatedDepreciation": -66760000000
    //          ....
    // "quarterly"
    //      "2020-06-30"
    //           ....
    //       "2020-09-30"
    //           ....
    //       "2020-12-31"
    //           ...
    // "trailing"
    //    ...

}
