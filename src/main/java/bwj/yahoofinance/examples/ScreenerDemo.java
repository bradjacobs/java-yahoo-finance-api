package bwj.yahoofinance.examples;

import bwj.yahoofinance.YahooFinanceClient;
import bwj.yahoofinance.http.HttpClientAdapterFactory;
import bwj.yahoofinance.request.YahooRequestBuilder;
import bwj.yahoofinance.request.builder.YahooFinanceRequest;
import bwj.yahoofinance.types.ScreenerField;
import com.jayway.jsonpath.JsonPath;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;


public class ScreenerDemo
{
    public static void main(String[] args) throws Exception
    {
        ScreenerDemo screenerDemo = new ScreenerDemo();
        //screenerDemo.screeenerRequest1();
        screenerDemo.screeenerRequest2();
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

    private void screeenerRequest2() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .setSize(100)
            .in(ScreenerField.REGION, Collections.singletonList("us"))
            .lt(ScreenerField.PERATIO, 20)
            .lt(ScreenerField.PRICEBOOKRATIO, 4)
            .gt(ScreenerField.ALTMANZSCOREUSINGTHEAVERAGESTOCKINFORMATIONFORAPERIOD, 3)
            .lt(ScreenerField.TOTALDEBTEQUITY, 110)
            .gt(ScreenerField.CURRENTRATIO, 1.5)
            .gt(ScreenerField.RETURNONEQUITY, 5)
            .gt(ScreenerField.NETINCOMEMARGIN, 7)
            .lt(ScreenerField.PEGRATIO_5Y, 1.1)
            .gt(ScreenerField.EODPRICE, 0.4)
            .build();

        String json = client.executeRequest(req);


        List<Map<String,Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].quotes");

        // print out results  --- PROOF OF CONCEPT ONLY ---
        for (Map<String, Object> entryMap : listOfMaps)
        {
            String symbol = (String) entryMap.get("symbol");
            Object name = entryMap.get("shortName");
            Object pe = entryMap.get("trailingPE");
            Object pb = entryMap.get("priceToBook");
            Object averageAnalystRating = entryMap.get("averageAnalystRating");

            String formattedStr = String.format("|%-5s| %-35s| %-12s| %-12s| %-18s|", symbol, name, pe, pb, averageAnalystRating);
            System.out.println(formattedStr);
        }

    }


}
