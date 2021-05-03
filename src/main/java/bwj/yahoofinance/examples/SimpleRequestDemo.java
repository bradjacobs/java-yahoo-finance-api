/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.enums.Interval;
import bwj.yahoofinance.enums.Type;
import bwj.yahoofinance.request.YahooFinanceRequest;
import bwj.yahoofinance.request.YahooLookupRequest;
import bwj.yahoofinance.request.YahooPriceHistoryRequest;

import static bwj.yahoofinance.enums.YahooEndpoint.*;
import static bwj.yahoofinance.enums.YahooModule.*;

public class SimpleRequestDemo
{
    public static void main(String[] args)
    {
        String testTicker = "AAPL";
        String testTicker2 = "CAT";

        String queryString = "AA*";

        SimpleRequestDemo requestDemo = new SimpleRequestDemo();

        try
        {
            //requestDemo.simpleRequest(testTicker);
            //requestDemo.multiEndpointRequest(testTicker);
            //requestDemo.quoteRequest(testTicker);
            //requestDemo.quoteRequestMultipleTicker(testTicker, testTicker2);
            //requestDemo.priceHistory(testTicker);
            requestDemo.basicLookupQuery(queryString);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }


    private void simpleRequest(String ticker)
    {
        // Query for Profile
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withModules(ASSET_PROFILE)
                        .withTicker(ticker);
        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void multiModuleRequest(String ticker)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(QUOTE_SUMMARY)
                        .withModules(ASSET_PROFILE)
                        .withModules(FINANCIAL_DATA)
                        .withModules(BALANCE_SHEET_HISTORY)
                        .withTicker(ticker);

        //  ... OR ...
        YahooFinanceRequest.Builder builderAlternate =
                new YahooFinanceRequest.Builder()
                        .withModules(ASSET_PROFILE, FINANCIAL_DATA, BALANCE_SHEET_HISTORY)
                        .withTicker(ticker);


        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void quoteRequest(String ticker)
    {
        // this will evolve over time.
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(QUOTE)
                        .withTicker(ticker).withTicker("CAT");

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void quoteRequestMultipleTicker(String ticker1, String ticker2)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(QUOTE)
                        .withTicker(ticker1)
                        .withTicker(ticker2);

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void basicLookupQuery(String query)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooLookupRequest.Builder builder =
                new YahooLookupRequest.Builder()
                        .withQuery(query)
                        .withType(Type.EQUITY)
                        .withFormatted(true)
                        .withCount(25)
                        .withStart(0);

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }


    private void priceHistory(String ticker)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooPriceHistoryRequest.Builder builder =
                new YahooPriceHistoryRequest.Builder()
                        .withTicker(ticker)
                        //.withRange(Range.FIVE_DAYS)
                        .withTimeRange("2021-01-25", "2021-01-28")
                        //.withLastXMonths(6)
                        //.withTimeRange(1619481600999L, 1619827200999L)
                        .withInterval(Interval.ONE_DAY);

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);

        // additional notes:
        //    &indicators=quote      (seems to do nothing?)
        //    &indicators=adjclose   (perhaps a yahoo bug, can return adjclose twice in some cases)
    }

}
