/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.http.HttpClientAdapterFactory;
import bwj.yahoofinance.request.YahooRequestBuilder;
import bwj.yahoofinance.types.Interval;
import bwj.yahoofinance.types.Region;
import bwj.yahoofinance.types.Type;
import bwj.yahoofinance.request.builder.YahooFinanceRequest;
import okhttp3.OkHttpClient;

import java.io.IOException;

import static bwj.yahoofinance.types.YahooEndpoint.*;
import static bwj.yahoofinance.types.YahooModule.*;

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
            requestDemo.simpleRequest(testTicker);
//            requestDemo.multiModuleRequest(testTicker);
//            requestDemo.quoteRequest(testTicker);
//            requestDemo.quoteRequestMultipleTicker(testTicker, testTicker2);
//            requestDemo.priceHistory(testTicker);
//            requestDemo.basicLookupQuery(queryString);
//            requestDemo.regionRequest(region);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }


    private void simpleRequest(String ticker) throws IOException
    {
        // Query for Profile
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createHttpClient(new OkHttpClient()));

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(ASSET_PROFILE)
            .withTicker(ticker)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void multiModuleRequest(String ticker) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(ASSET_PROFILE)
            .withModules(FINANCIAL_DATA)
            .withModules(BALANCE_SHEET_HISTORY)
            .withTicker(ticker)
            .build();


        //  ... OR ...
        YahooFinanceRequest reqAlternative = YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(ASSET_PROFILE, FINANCIAL_DATA, BALANCE_SHEET_HISTORY)
            .withTicker(ticker)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void quoteRequest(String ticker) throws IOException
    {
        // this will evolve over time.
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .quote()
            .withTicker(ticker)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void quoteRequestMultipleTicker(String ticker1, String ticker2) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .quote()
            .withTicker(ticker1)
            .withTicker(ticker2)
            .build();

        String json = client.executeRequest(req);

        YahooFinanceRequest reqAlternate = YahooRequestBuilder.api()
            .quote()
            .withTicker(ticker1, ticker2)
            .build();

        String jsonAlternate = client.executeRequest(reqAlternate);

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

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }


    private void priceHistory(String ticker) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .priceHistory()
            .withTicker(ticker)
            //.withRange(Range.FIVE_DAYS)
            .withTimeRange("2021-01-25", "2021-01-28")
            //.withLastXMonths(6)
            //.withTimeRange(1619481600999L, 1619827200999L)
            .withInterval(Interval.ONE_DAY)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);

        // additional notes:
        //    &indicators=quote      (seems to do nothing?)
        //    &indicators=adjclose   (perhaps a yahoo bug, can return adjclose twice in some cases)
    }


    private void regionRequest(Region region) throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .endpointRequest(MARKET_SUMMARY)
            .withRegion(region)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

}
