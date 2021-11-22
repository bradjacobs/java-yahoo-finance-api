package com.github.bradjacobs.yahoofinance.demo.lookup;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.LookupResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.lookup.LookupRequestExample.SIMPLE;

public class LookupRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        YahooRequest req = SIMPLE.getRequest();
        lookupRequestRunner(req);
    }

    private static void lookupRequestRunner(YahooRequest req) throws IOException
    {
        if (req == null || !(req.getEndpoint().equals(YahooEndpoint.LOOKUP) || req.getEndpoint().equals(YahooEndpoint.LOOKUP_TOTALS))) {
            throw new IllegalArgumentException("Must supply a lookup-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient();
        YahooResponse resp = client.execute(req);

        // get original json response
        String rawJson = resp.getJson();

        // get original json response (in pretty format)
        String prettyJson = resp.getPrettyJson();

        // results as list of key/value pairs
        List<Map<String, Object>> listOfMaps = resp.getAsListOfMaps();

        // same result as listOfMaps, but in a map where the 'key' is the ticker/symbol value.
        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();

        // get list results in form of predefined class.
        List<LookupResult> lookupResultList = resp.getAsListOfPojos(LookupResult.class);

        // get map results in form of predefined class (key is the ticker/symbol)
        Map<String, LookupResult> lookupResultMap = resp.getAsMapOfPojos(LookupResult.class);

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

}
