package com.github.bradjacobs.yahoofinance.demo.chart;


import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.YahooFinanceObjectClient;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.ChartResult;
import com.github.bradjacobs.yahoofinance.model.ScreenerResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class ChartRequests
{
    public static void main(String[] args) throws Exception
    {
        ChartRequests chartDemo = new ChartRequests();
        chartDemo.basicJsonRequest();

        YahooFinanceRequest req = ChartRequestDemoFactory.getRequest(3);

        chartDemo.chartRequestRunner(req);
    }


    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();

    public void chartRequestRunner(YahooFinanceRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.CHART)) {
            throw new IllegalArgumentException("Must supply a chart-type request");
        }

        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        YahooResponse resp = client.execute(req);

        String rawJson = resp.getJson();
        String prettyJson = resp.getPrettyJson();
        List<Map<String,Object>> listOfMaps = resp.getAsListOfMaps();


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

        int kjkk = 3333;


    }


    /**
     * Query
     */
    public void basicJsonRequest() throws IOException
    {

    }


}
