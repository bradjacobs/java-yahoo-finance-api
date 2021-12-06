package com.github.bradjacobs.yahoofinance.demo.visualization;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.model.IpoEventResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.demo.visualization.IpoEventRequestExample.SIMPLE;

public class VisualizationIpoRequesterDemo
{
    public static void main(String[] args) throws Exception
    {
        YahooRequest req = SIMPLE.getRequest();
        visualRequestRunner(req);
    }


    private static void visualRequestRunner(YahooRequest req) throws IOException
    {
        if (req == null || !req.getEndpoint().equals(YahooEndpoint.VISUALIZATION)) {
            throw new IllegalArgumentException("Must supply a visualization-type request");
        }

        try
        {
            YahooFinanceClient client = new YahooFinanceClient();
            YahooResponse resp = client.execute(req);

            // get original json response
            String rawJson = resp.getJson();

            // get original json response (in pretty format)
            String prettyJson = resp.getPrettyJson();

            List<Map<String, Object>> listOfMaps = resp.getAsListOfMaps();

            List<IpoEventResult> pojoList = resp.getAsListOfPojos(IpoEventResult.class);

            // get map results in form of predefined class (key is the ticker/symbol)
            Map<String, IpoEventResult> pojoMap = resp.getAsMapOfPojos(IpoEventResult.class);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

}
