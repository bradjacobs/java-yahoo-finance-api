package com.github.bradjacobs.yahoofinance.examples;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.validation.YahooFinanceMapClient;

import java.io.IOException;
import java.util.List;
import java.util.Map;


public class ChartDemo
{
    public static void main(String[] args) throws Exception
    {
        ChartDemo chartDemo = new ChartDemo();
        chartDemo.screeenerRequestMapClient();
    }

    private void screeenerRequestMapClient() throws IOException
    {
        YahooFinanceClient baseClient = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());
        YahooFinanceMapClient client = new YahooFinanceMapClient(baseClient);

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

        List<Map<String,Object>> listOfMaps = client.executeRequest(req);

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

}
