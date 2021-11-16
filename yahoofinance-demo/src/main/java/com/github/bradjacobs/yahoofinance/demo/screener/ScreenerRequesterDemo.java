package com.github.bradjacobs.yahoofinance.demo.screener;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.ScreenerResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.screener.ScreenerRequestExample.TOP_MARKET_CAP;

public class ScreenerRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        YahooFinanceRequest req = TOP_MARKET_CAP.getRequest();
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

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

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
