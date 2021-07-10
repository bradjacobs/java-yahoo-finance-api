package com.github.bradjacobs.yahoofinance.demo.quote;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.QuoteResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class QuoteRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 1;

        YahooFinanceRequest req = QuoteRequestDemoFactory.getRequest(exampleRequestId);

        quoteRequestRunner(req);
    }


    private static void quoteRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.QUOTE)) {
            throw new IllegalArgumentException("Must supply a quote-type request");
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
        List<QuoteResult> screenerResultList = resp.getAsListOfPojos(QuoteResult.class);

        // get map results in form of predefined class (key is the ticker/symbol)..
        //    (not too useful if only querying for a single ticker value)
        Map<String, QuoteResult> screenerResultMap = resp.getAsMapOfPojos(QuoteResult.class);

        System.out.println("Total Result Count: " + mapsOfMaps.size());

    }

}
