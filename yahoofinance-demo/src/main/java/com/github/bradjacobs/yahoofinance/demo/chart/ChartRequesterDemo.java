package com.github.bradjacobs.yahoofinance.demo.chart;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.ChartResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.chart.ChartRequestExample.SIMPLE;

public class ChartRequesterDemo
{
    private static final EpochSecondsConverter EPOCH_DATE_STR_CONVERTER = new EpochSecondsConverter();

    public static void main(String[] args) throws Exception
    {
        chartRequestRunner(SIMPLE.getRequest());
    }

    private static void chartRequestRunner(YahooRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.CHART)) {
            throw new IllegalArgumentException("Must supply a chart-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        YahooResponse resp = client.execute(req);

        String rawJson = resp.getJson();
        String prettyJson = resp.getPrettyJson();
        List<Map<String,Object>> listOfMaps = resp.getAsListOfMaps();
        Map<String, Map<String, Object>> mapOfMaps = resp.getAsMapOfMaps();

        // convert into special custom class type
        List<ChartResult> listofChartResults = resp.getAsListOfPojos(ChartResult.class);

        //printListOfMaps(listOfMaps);
        printChartResults(listofChartResults);
    }

    private static void printChartResults(List<ChartResult> chartResults)
    {
        for (ChartResult chartResult : chartResults)
        {
            String date = chartResult.getDate();
            Double close = chartResult.getClose();
            Double adjclose = chartResult.getAdjclose();

            String formattedStr = null;
            if (adjclose != null) {
                formattedStr = String.format("| %-11s| %-8.2f| %-8.2f|", date, close, adjclose);
            }
            else {
                formattedStr = String.format("| %-11s| %-8.2f|", date, close);
            }
            System.out.println(formattedStr);
        }
    }


    private static void printListOfMaps(List<Map<String,Object>> listOfMaps)
    {
        // print out results  --- PROOF OF CONCEPT ONLY ---
        for (Map<String, Object> entryMap : listOfMaps)
        {
            Object timestamp = entryMap.get("timestamp");
            Object close = entryMap.get("close");
            Object adjclose = entryMap.get("adjclose");

            String formattedStr = null;
            if (adjclose != null) {
                formattedStr = String.format("| %-11s| %-8.2f| %-8.2f|", EPOCH_DATE_STR_CONVERTER.toString((Long)timestamp), (Double)close, (Double)adjclose);
            }
            else {
                formattedStr = String.format("| %-11s| %-8.2f|", EPOCH_DATE_STR_CONVERTER.toString((Long)timestamp), (Double)close);
            }
            System.out.println(formattedStr);
        }
    }

}
