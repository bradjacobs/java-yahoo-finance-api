package com.github.bradjacobs.yahoofinance.demo.spark;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.spark.SparkRequestExample.MULTI;

public class SparkRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        YahooRequest req = MULTI.getRequest();
        sparkRequestRunner(req);
    }


    private static void sparkRequestRunner(YahooRequest req) throws IOException
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
