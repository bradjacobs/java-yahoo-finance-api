package com.github.bradjacobs.yahoofinance.demo.spark;


import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.demo.misc.objects.KeyStatistics;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.spark.SparkRequestExample.MULTI;

public class SparkRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        YahooRequest req = MULTI.getRequest();
        //sparkRequestRunner(req);

        batchFetch();
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

    public static void batchFetch() throws Exception
    {
        String stockFilePath = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-demo/src/main/java/com/github/bradjacobs/yahoofinance/demo/spark/stocks.json";
        String stockPricesPath = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-demo/src/main/java/com/github/bradjacobs/yahoofinance/demo/spark/stockprices2.json";

        JsonMapper mapper = JsonMapperSingleton.getPrettyInstance();
        String[] stockTickers = mapper.readValue(new File(stockFilePath), String[].class);

        YahooRequest req = YahooRequestBuilder.api()
                .spark()
                .withTicker(stockTickers)
                .withRange(Range.ONE_YEAR)
                .withInterval(Interval.ONE_DAY)
                .build();

        YahooFinanceClient client = new YahooFinanceClient();
        YahooResponse resp = client.execute(req);

        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();
        mapper.writeValue(new File(stockPricesPath), mapsOfMaps);

        Map<String,Map<String,Double>> stockPriceMap = mapper.readValue(new File(stockPricesPath), new TypeReference<Map<String,Map<String,Double>>>() {});


        int indddd = 333;

    }

}
