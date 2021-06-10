package com.github.bradjacobs.yahoofinance.examples;


import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.YahooFinanceObjectClient;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.model.PriceHistoryRecord;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;

import java.io.IOException;
import java.util.List;
import java.util.Map;


public class ChartDemo
{
    public static void main(String[] args) throws Exception
    {
        ChartDemo chartDemo = new ChartDemo();
        //chartDemo.screeenerRequestMapClient();
        chartDemo.screeenerRequestObjectClient();
    }





    private void screeenerRequestMapClient() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .priceHistory()
            .withTicker("AAPL")
            .withRange(Range.ONE_MONTH)
            .withInterval(Interval.ONE_DAY)
            .withDividends(true)
            .withSplits(true)
            .withIndicatorAllFields()
            .build();

        // todo: note the 'withDividends' and 'withSplits' will not be included in the final map response
        //    (at least for now).  Need to 'reduce the confusion'

        List<Map<String,Object>> listOfMaps = client.executeListRequest(req);

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

        /*
            | 2021-05-10 | 126.85  | 126.85  |
            | 2021-05-11 | 125.91  | 125.91  |
            | 2021-05-12 | 122.77  | 122.77  |
            | 2021-05-13 | 124.97  | 124.97  |
            .....
         */
    }
    private void screeenerRequestObjectClient() throws IOException
    {
        // todo: obviously need a better way to create these.
        YahooFinanceClient baseClient = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());
        YahooFinanceObjectClient client = new YahooFinanceObjectClient(baseClient);

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .priceHistory()
            .withTicker("AAPL")
            .withRange(Range.ONE_MONTH)
            .withInterval(Interval.ONE_DAY)
            .withIndicatorAllFields()
            .build();

        //List<PriceHistoryRecord> recordList = client.fetchObjects(req, PriceHistoryRecord.class);
        Map<String, PriceHistoryRecord> recordList2 = client.fetchMappedObjects(req, PriceHistoryRecord.class);

        int kjk = 3333;
       // System.out.println("Record count: " + recordArray.length);

    }

}
