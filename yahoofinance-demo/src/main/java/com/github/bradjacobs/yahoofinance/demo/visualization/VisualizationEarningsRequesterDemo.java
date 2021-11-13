package com.github.bradjacobs.yahoofinance.demo.visualization;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.beta.EarningsEventResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.EarningsEventRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.*;

public class VisualizationEarningsRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        EarningsEventRequestBuilder earningsEventRequestBuilder = new EarningsEventRequestBuilder();
        String date = "2021-11-01";
        earningsEventRequestBuilder.setStart(date);

        YahooFinanceRequest req = earningsEventRequestBuilder.build();

        visualRequestRunner(req);
    }


    private static void visualRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.VISUALIZATION)) {
            throw new IllegalArgumentException("Must supply a visualization-type request");
        }

        try
        {
            boolean tryBatch = true;

            YahooFinanceClient client = new YahooFinanceClient();


            if (tryBatch)
            {
                YahooBatchResponse batchResp = client.executeBatch(req);

                List<String> jsonList = batchResp.getJson();

                List<EarningsEventResult> pojoList = batchResp.getAsListOfPojos(EarningsEventResult.class);

                Map<String, EarningsEventResult> tickerMap = new HashMap<>();

                for (EarningsEventResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    EarningsEventResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }

                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, EarningsEventResult> pojoMap = batchResp.getAsMapOfPojos(EarningsEventResult.class);

                int kjjjj = 3333;

            }
            else {
                YahooResponse resp = client.execute(req);

                // get original json response
                String rawJson = resp.getJson();

                // get original json response (in pretty format)
                String prettyJson = resp.getPrettyJson();

                List<Map<String, Object>> listOfMaps = resp.getAsListOfMaps();

                List<EarningsEventResult> pojoList = resp.getAsListOfPojos(EarningsEventResult.class);

                Set<String> extraNames = new TreeSet<>();

                Map<String, EarningsEventResult> tickerMap = new HashMap<>();

                for (EarningsEventResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    String type = pojo.getEventType();
                    String eventName = pojo.getEventName();

                    if (type != null) {
                        int kjkjj = 333;
                    }
                    if (eventName != null) {
                        int kjkkjjk = 333;
                    }

                    Map<String, Object> propMap = pojo.getAdditionalProperties();
                    extraNames.addAll(propMap.keySet());

                    EarningsEventResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }

                for (String extraName : extraNames) {
                    System.out.println(extraName);
                }

                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, EarningsEventResult> pojoMap = resp.getAsMapOfPojos(EarningsEventResult.class);


                int kjjjj = 333;
            }

            int kjkjj = 333;

        }
        catch (Exception e)
        {
            e.printStackTrace();
            int kjj = 33;
        }

//        // same result as listOfMaps, but in a map where the 'key' is the ticker/symbol value.
//        Map<String, Map<String, Object>> mapsOfMaps = resp.getAsMapOfMaps();
//
//        System.out.println("Total Result Count: " + mapsOfMaps.size());
    }

}
