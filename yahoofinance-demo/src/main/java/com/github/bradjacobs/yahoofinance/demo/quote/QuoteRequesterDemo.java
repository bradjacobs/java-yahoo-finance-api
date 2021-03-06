package com.github.bradjacobs.yahoofinance.demo.quote;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.QuoteResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.quote.QuoteRequestExample.*;

public class QuoteRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        String ss = "AAPL,MSFT,GOOG,AMZN,TSLA,FB,JPM,V,HD,UNH,JNJ,WMT";
        List<String> tickerList = Arrays.asList(ss.split(","));

        YahooRequest req = YahooRequestBuilder.api()
                .quote()
                .withTicker(tickerList)
                .build();

//        YahooRequest req = YahooRequestBuilder.api()
//                .validate()
//                .withTicker(tickerList)
//                .build();

        ///YahooRequest req = SIMPLE.getRequest();
        quoteRequestRunner(req);
    }

    private static void quoteRequestRunner(YahooRequest req) throws IOException
    {
//        if (req == null || !req.getEndpoint().equals(YahooEndpoint.QUOTE)) {
//            throw new IllegalArgumentException("Must supply a quote-type request");
//        }

        //YahooFinanceClient client = new YahooFinanceClient();
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

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
        List<QuoteResult> quoteResultList = resp.getAsListOfPojos(QuoteResult.class);

        // get map results in form of predefined class (key is the ticker/symbol)..
        //    (not too useful if only querying for a single ticker value)
        Map<String, QuoteResult> quoteResultMap = resp.getAsMapOfPojos(QuoteResult.class);

        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

}
