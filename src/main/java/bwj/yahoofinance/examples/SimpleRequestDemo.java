/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooEndpoint;
import bwj.yahoofinance.YahooFinanceClient;

public class SimpleRequestDemo
{
    public static void main(String[] args)
    {
        try
        {
            // Query for Apple's Profile
            YahooFinanceClient client = new YahooFinanceClient();
            String json = client.executeRequest("AAPL", YahooEndpoint.ASSET_PROFILE);

            System.out.println("--JSON RESPONSE--");
            System.out.println(json);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }

    }
}
