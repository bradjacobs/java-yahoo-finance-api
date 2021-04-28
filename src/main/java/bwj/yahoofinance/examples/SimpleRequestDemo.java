/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.model.request.YahooFinanceRequest;
import bwj.yahoofinance.model.request.YahooLookupRequest;

import static bwj.yahoofinance.YahooEndpoint.*;

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
            requestDemo.simpleRequest(testTicker);
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
                        .withEndpoint(ASSET_PROFILE)
                        .withTicker(ticker);
        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void multiEndpointRequest(String ticker)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(ASSET_PROFILE)
                        .withEndpoint(FINANCIAL_DATA)
                        .withEndpoint(BALANCE_SHEET_HISTORY)
                        .withTicker(ticker);

        //  ... OR ...
        YahooFinanceRequest.Builder builderAlternate =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(ASSET_PROFILE, FINANCIAL_DATA, BALANCE_SHEET_HISTORY)
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
                        .withFormatted(true)
                        .withCount(20)
                        .withStart(0);

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }


    private void priceHistory(String ticker)
    {
        YahooFinanceClient client = new YahooFinanceClient();

        YahooFinanceRequest.Builder builder =
                new YahooFinanceRequest.Builder()
                        .withEndpoint(CHART)
                        .withTicker(ticker);

        // NOTE: it is possible to query chart endpoint w/o any additional parameters
        //   but the 'default' values used seem a bit unintuitive imho

        builder.addParameter("range", "5d");
        builder.addParameter("interval", "1d");

        // can be used instead of range
//        builder.addParameter("period1", "1619222400");
//        builder.addParameter("period2", "1619222400");

        // below can alter what data is returned
//        builder.addParameter("indicators", "close");           // ONLY return close and adjclose
//        builder.addParameter("includeTimestamps", "false");    // do NOT return timestamps
//        builder.addParameter("includeAdjustedClose", "false"); // do NOT return adj close (default is true)

//        builder.addParameter("events", "div");  // also include dividends (if exists in timerange)
//        builder.addParameter("events", "split");  // also include stock splits (if exists in timerange)
//        builder.addParameter("events", "div,split");  // also include BOTH dividends & stock splits (if exists in timerange)


        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);

        // additional notes:
        //    &indicators=quote      (seems to do nothing)
        //    &indicators=adjclose   (perhaps a yahoo bug, returns adjclose twice)
        //    &includePrePost=true   (seen mentioned many times, but doesn't seem actually to do anything?)
    }


}
