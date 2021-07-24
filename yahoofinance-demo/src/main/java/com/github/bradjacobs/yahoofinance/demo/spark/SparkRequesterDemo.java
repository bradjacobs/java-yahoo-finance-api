package com.github.bradjacobs.yahoofinance.demo.spark;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.LookupResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class SparkRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 2;

        YahooFinanceRequest req = SparkRequestDemoFactory.getRequest(exampleRequestId);

        soarkRequestRunner(req);
    }


    private static void soarkRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.SPARK)) {
            throw new IllegalArgumentException("Must supply a spark-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient();
        YahooResponse resp = client.execute(req);

        // get original json response
        String rawJson = resp.getJson();

        // get original json response (in pretty format)
        String prettyJson = resp.getPrettyJson();
        
        // same result as listOfMaps, but in a map where the 'key' is the ticker/symbol value.
        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

}
