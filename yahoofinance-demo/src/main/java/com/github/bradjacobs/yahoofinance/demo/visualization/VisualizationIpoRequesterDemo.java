package com.github.bradjacobs.yahoofinance.demo.visualization;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.IpoEventResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.IpoEventRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.batch.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class VisualizationIpoRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        IpoEventRequestBuilder ipoRequestBuilder = new IpoEventRequestBuilder();
        //String date = "2021-11-17";
        String date = "2021-11-02";
        ipoRequestBuilder.setStart(date);

        YahooFinanceRequest req = ipoRequestBuilder.build();

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

                List<IpoEventResult> pojoList = batchResp.getAsListOfPojos(IpoEventResult.class);

                Map<String, IpoEventResult> tickerMap = new HashMap<>();

                for (IpoEventResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    IpoEventResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }

                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, IpoEventResult> pojoMap = batchResp.getAsMapOfPojos(IpoEventResult.class);

                int kjjjj = 3333;

            }
            else {
                YahooResponse resp = client.execute(req);

                // get original json response
                String rawJson = resp.getJson();

                // get original json response (in pretty format)
                String prettyJson = resp.getPrettyJson();

                List<Map<String, Object>> listOfMaps = resp.getAsListOfMaps();

                List<IpoEventResult> pojoList = resp.getAsListOfPojos(IpoEventResult.class);

                Set<String> extraNames = new TreeSet<>();

                Map<String, IpoEventResult> tickerMap = new HashMap<>();

                for (IpoEventResult pojo : pojoList) {
                    String ticker = pojo.getTicker();

                    Map<String, Object> propMap = pojo.getAdditionalProperties();
                    extraNames.addAll(propMap.keySet());

                    IpoEventResult existingResult = tickerMap.get(ticker);
                    if (existingResult != null) {
                        int kjkjjj = 3333;
                    }

                    tickerMap.put(ticker, pojo);
                }

                for (String extraName : extraNames) {
                    System.out.println(extraName);
                }

                // get map results in form of predefined class (key is the ticker/symbol)
                Map<String, IpoEventResult> pojoMap = resp.getAsMapOfPojos(IpoEventResult.class);


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
