package com.github.bradjacobs.yahoofinance.demo.screener;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.ScreenerResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class ScreenerRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        // choose a number that represents the request to try (from the 'ScreenerRequestDemoFactory')
        int exampleRequestId = 3;

        YahooFinanceRequest req = ScreenerRequestDemoFactory.getRequest(exampleRequestId);

        screenerRequestRunner(req);
    }


    private static void screenerRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !(req.getEndpoint().equals(YahooEndpoint.SCREENER) || req.getEndpoint().equals(YahooEndpoint.SCREENER_TOTALS))) {
            throw new IllegalArgumentException("Must supply a screener-type request");
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
        List<ScreenerResult> screenerResultList = resp.getAsListOfPojos(ScreenerResult.class);

        // get map results in form of predefined class (key is the ticker/symbol)
        Map<String, ScreenerResult> screenerResultMap = resp.getAsMapOfPojos(ScreenerResult.class);

        Set<String> exchanges = new TreeSet<>();
        for (ScreenerResult result : screenerResultList)
        {
            String ex = result.getExchange();
            if (ex != null) {
                exchanges.add(ex);
            }
        }

        for (String exchange : exchanges)
        {
            System.out.println("EX -- " + exchange);
        }


//        EX -- NMS
//        EX -- NYQ
//        EX -- PNK

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

    /*

    "PYTCF" -> {ScreenerResult@2504}
"BMBLF" -> {ScreenerResult@2506}
"AAPL" -> {ScreenerResult@2508}
"MSFT" -> {ScreenerResult@2510}
"AMZN" -> {ScreenerResult@2512}
"GOOG" -> {ScreenerResult@2514}
"GOOGL" -> {ScreenerResult@2516}
"FB" -> {ScreenerResult@2518}
"TCEHY" -> {ScreenerResult@2520}
"TCTZF" -> {ScreenerResult@2522}
     */

    /**
     * Prints out a few chosen values.
     * @param listOfMaps
     */
    private static void printSomeListValues(List<Map<String, Object>> listOfMaps)
    {
        for (Map<String, Object> entryMap : listOfMaps)
        {
            String symbol = (String) entryMap.get("symbol");
            Object name = entryMap.get("shortName");
            Object pe = entryMap.get("trailingPE");
            Object pb = entryMap.get("priceToBook");

            String formattedStr = String.format("|%-5s| %-35s| %-12s| %-12s|", symbol, name, pe, pb);
            System.out.println(formattedStr);
        }

    }


}
