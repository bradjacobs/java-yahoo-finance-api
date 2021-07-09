/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.examples;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.exception.HttpClientErrorException;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.Type;
import okhttp3.OkHttpClient;

import java.io.IOException;

import static com.github.bradjacobs.yahoofinance.types.YahooEndpoint.MARKET_SUMMARY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.ASSET_PROFILE;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.BALANCE_SHEET_HISTORY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.FINANCIAL_DATA;

// TODO - redo this class (split up examples)
public class SimpleRequestDemo
{
    public static void main(String[] args)
    {
        String testTicker = "MSFT";
        String testTicker2 = "CAT";

        String queryString = "AA*";
        Region region = Region.FRANCE;

        SimpleRequestDemo requestDemo = new SimpleRequestDemo();

        try
        {
//            equestDemo.quoteRequest(testTicker);
//            requestDemo.quoteRequestMultipleTicker(testTicker, testTicker2);
//            requestDemo.basicLookupQuery(queryString);
//            requestDemo.regionRequest(region);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }



    private void quoteRequest(String ticker) throws IOException
    {
        // this will evolve over time.
        YahooFinanceClient client = new YahooFinanceClient();

        try
        {
            YahooFinanceRequest req = YahooRequestBuilder.api()
                .quote()
                .withTicker(ticker)
                .build();

            YahooResponse resp = client.execute(req);
            String json = resp.getJson();

            System.out.println("--JSON RESPONSE--");
            System.out.println(json);
        }
        catch (Exception e) {
            e.printStackTrace();
            int kjkj = 33333;
        }
    }

    private void quoteRequestMultipleTicker(String ticker1, String ticker2) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .quote()
            .withTicker(ticker1)
            .withTicker(ticker2)
            .build();

        YahooResponse resp = client.execute(req);
        String json = resp.getJson();

        YahooFinanceRequest reqAlternate = YahooRequestBuilder.api()
            .quote()
            .withTicker(ticker1, ticker2)
            .build();

        YahooResponse respAlternate = client.execute(reqAlternate);
        String jsonAlternate = resp.getJson();

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void basicLookupQuery(String query) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .lookup()
            .withQuery(query)
            .withType(Type.EQUITY)
            .withFormatted(true)
            .withCount(25)
            .withStart(0)
            .build();

        YahooResponse resp = client.execute(req);
        String json = resp.getJson();

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }




    private void regionRequest(Region region) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .endpointRequest(MARKET_SUMMARY)
            .withRegion(region)
            .build();

        YahooResponse resp = client.execute(req);
        String json = resp.getJson();

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

}
