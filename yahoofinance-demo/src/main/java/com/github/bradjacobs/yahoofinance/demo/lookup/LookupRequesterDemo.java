package com.github.bradjacobs.yahoofinance.demo.lookup;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.LookupResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class LookupRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 1;

        YahooFinanceRequest req = LookupRequestDemoFactory.getRequest(exampleRequestId);

        lookupRequestRunner(req);
    }


    private static void lookupRequestRunner(YahooFinanceRequest req) throws IOException
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


    // example response:
    /*
            {
              "finance": {
                "result": [
                  {
                    "start": 0,
                    "count": 5,
                    "total": 3,
                    "documents": [
                      {
                        "symbol": "MSFT",
                        "shortName": "Microsoft Corporation",
                        "regularMarketPrice": 277.94,
                        "regularMarketChange": 0.519989,
                        "regularMarketPercentChange": 0.18743746,
                        "industryName": "Technology",
                        "quoteType": "equity",
                        "exchange": "NMS",
                        "industryLink": "https://finance.yahoo.com/sector/technology",
                        "rank": 367763
                      },
                      {
                        "symbol": "1MSF.PA",
                        "shortName": "1X MSFT",
                        "regularMarketPrice": 4.6585,
                        "regularMarketChange": 0.0064001083,
                        "regularMarketPercentChange": 0.13757461,
                        "quoteType": "equity",
                        "exchange": "PAR",
                        "rank": 20001
                      },
                      {
                        "symbol": "PB2661.MI",
                        "shortName": "BPA TB SH MSFT 300,0 B300,0 OP ",
                        "regularMarketPrice": 2.83,
                        "regularMarketChange": -0.035000086,
                        "regularMarketPercentChange": -1.2216434,
                        "quoteType": "equity",
                        "exchange": "MIL"
                      }
                    ]
                  }
                ],
                "error": null
              }
            }
     */
}
