package com.github.bradjacobs.yahoofinance.demo.visualization;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.demo.spark.SparkRequestDemoFactory;
import com.github.bradjacobs.yahoofinance.model.LookupResult;
import com.github.bradjacobs.yahoofinance.model.beta.EarningsVisualizationResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.EarningsRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.*;

public class VisualizationRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        EarningsRequestBuilder earningsRequestBuilder = new EarningsRequestBuilder();
        String date = "2021-11-01";
        earningsRequestBuilder.setStart(date);

        YahooFinanceRequest req = earningsRequestBuilder.build();

        visualRequestRunner(req);
    }


    private static void visualRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.VISUALIZATION)) {
            throw new IllegalArgumentException("Must supply a visualization-type request");
        }

        try
        {
            boolean tryBatch = false;

            YahooFinanceClient client = new YahooFinanceClient();


            if (tryBatch)
            {
                YahooBatchResponse batchResp = client.executeBatch(req);

                List<String> jsonList = batchResp.getJson();

                List<EarningsVisualizationResult> pojoList = batchResp.getAsListOfPojos(EarningsVisualizationResult.class);

                Map<String, EarningsVisualizationResult> tickerMap = new HashMap<>();

                for (EarningsVisualizationResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    EarningsVisualizationResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }


                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, EarningsVisualizationResult> pojoMap = batchResp.getAsMapOfPojos(EarningsVisualizationResult.class);

                int kjjjj = 3333;

            }
            else {
                YahooResponse resp = client.execute(req);

                // get original json response
                String rawJson = resp.getJson();

                // get original json response (in pretty format)
                String prettyJson = resp.getPrettyJson();

                List<Map<String, Object>> listOfMaps = resp.getAsListOfMaps();

                List<EarningsVisualizationResult> pojoList = resp.getAsListOfPojos(EarningsVisualizationResult.class);

                Map<String, EarningsVisualizationResult> tickerMap = new HashMap<>();

                for (EarningsVisualizationResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    EarningsVisualizationResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }

                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, EarningsVisualizationResult> pojoMap = resp.getAsMapOfPojos(EarningsVisualizationResult.class);


                int kjjjj = 333;

            }




            int kjkjj = 333;

        }
        catch (Exception e)
        {
            int kjj = 33;
        }

//        // same result as listOfMaps, but in a map where the 'key' is the ticker/symbol value.
//        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();
//
//        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

}
