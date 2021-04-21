/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.YahooFinanceRequest;

import static bwj.yahoofinance.YahooEndpoint.*;

public class SimpleRequestDemo
{
    public static void main(String[] args)
    {
        String testTicker = "AAPL";
        SimpleRequestDemo requestDemo = new SimpleRequestDemo();

        try
        {
            //requestDemo.simpleRequest(testTicker);
            //requestDemo.multiEndpointRequest(testTicker);
            requestDemo.quoteRequest(testTicker);
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
                        .withTicker(ticker);

        String json = client.executeRequest(builder.build());

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

}
