package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.http.HttpClientAdapterFactory;
import bwj.yahoofinance.request.YahooRequestBuilder;
import bwj.yahoofinance.request.builder.YahooFinanceRequest;
import bwj.yahoofinance.types.ScreenerField;

import java.io.IOException;
import java.util.Collections;


public class ScreenerDemo
{
    public static void main(String[] args) throws Exception
    {
        ScreenerDemo screenerDemo = new ScreenerDemo();
        screenerDemo.screeenerRequest1();
    }

    private void screeenerRequest1() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Collections.singletonList("us"))
            .gt(ScreenerField.LEVEREDFREECASHFLOW1YRGROWTH, 2)
            .gt(ScreenerField.NETINCOME1YRGROWTH, 5)
            .build();

        String json = client.executeRequest(req);

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

}
