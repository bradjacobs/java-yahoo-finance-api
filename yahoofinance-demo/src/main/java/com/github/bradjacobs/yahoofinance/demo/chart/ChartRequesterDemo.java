package com.github.bradjacobs.yahoofinance.demo.chart;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.ChartResult;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class ChartRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        int exampleRequestId = 1;

        YahooFinanceRequest req = ChartRequestDemoFactory.getRequest(exampleRequestId);

        chartRequestRunner(req);
    }


    private static void chartRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.CHART)) {
            throw new IllegalArgumentException("Must supply a chart-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        YahooResponse resp = client.execute(req);

        String rawJson = resp.getJson();
        String prettyJson = resp.getPrettyJson();
        List<Map<String,Object>> listOfMaps = resp.getAsListOfMaps();

        // convert into special custom class type
        List<ChartResult> listofChartResults = resp.getAsListOfPojos(ChartResult.class);

        printListOfMaps(listOfMaps);
    }

    private static void printListOfMaps(List<Map<String,Object>> listOfMaps)
    {
        EpochSecondsConverter epochConverter = new EpochSecondsConverter();

        // print out results  --- PROOF OF CONCEPT ONLY ---
        for (Map<String, Object> entryMap : listOfMaps)
        {
            Object timestamp = entryMap.get("timestamp");
            Object close = entryMap.get("close");
            Object adjclose = entryMap.get("adjclose");

            String formattedStr = String.format("| %-11s| %-8.2f| %-8.2f|", epochConverter.convertToString((Long)timestamp), (Double)close, (Double)adjclose);
            System.out.println(formattedStr);
        }
    }

}
