package com.github.bradjacobs.yahoofinance.examples;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.YahooFinanceObjectClient;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.TimeSeriesResult;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;

import java.io.IOException;
import java.util.List;


public class TimeSeriesDemo
{
    public static void main(String[] args) throws Exception
    {
        TimeSeriesDemo timeseriesDemo = new TimeSeriesDemo();
        timeseriesDemo.timeseriesRequest1();
    }

    private void timeseriesRequest1() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());
        YahooFinanceObjectClient pbjectClient = new YahooFinanceObjectClient(client);

        // still _VERY_ beta
// &period1=493590046&period2=1619487910&corsDomain=finance.yahoo.com

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .timeSeries()
            .withTicker("AAPL")
            .withStart(1501100800L)
            .withEnd(1618790400L)
            .withPadTimeSeries(true)
            .build();

//        String json = client.executeRequest(req);
//        List<Map<String, Object>> listofMaps = client.executeListRequest(req);
        List<TimeSeriesResult> listResult = pbjectClient.fetchObjects(req, TimeSeriesResult.class);

        System.out.println("Done!");
    }
}
