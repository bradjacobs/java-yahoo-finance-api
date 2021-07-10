package com.github.bradjacobs.yahoofinance.demo.quotesummary;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class QuoteSummaryRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 3;

        YahooFinanceRequest req = QuoteSummaryRequestDemoFactory.getRequest(exampleRequestId);

        quoteSummaryRequestRunner(req);
    }


    private static void quoteSummaryRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.QUOTE_SUMMARY)) {
            throw new IllegalArgumentException("Must supply a quotesummery-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient();
        YahooResponse resp = client.execute(req);

        String rawJson = resp.getJson();
        String prettyJson = resp.getPrettyJson();


        // note: the format of response is dependent on which modules were requested.
        List<Map<String,Object>> listOfMaps = resp.getAsListOfMaps();
        Map<String, Map<String, Object>> mapOfMaps = resp.getAsMapOfMaps();


        System.out.println("QuoteSummary JSON Response (pretty form) ...");
        System.out.println(prettyJson);

    }

}
