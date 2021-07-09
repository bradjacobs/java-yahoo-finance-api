package com.github.bradjacobs.yahoofinance.examples;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.request.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.jayway.jsonpath.JsonPath;

import java.io.IOException;
import java.util.List;
import java.util.Map;


public class ScreenerDemo
{
    public static void main(String[] args) throws Exception
    {
        ScreenerDemo screenerDemo = new ScreenerDemo();
        //screenerDemo.screeenerRequest1();
        //screenerDemo.screeenerRequestTotalOnly();
        screenerDemo.screeenerRequest2();
        //screenerDemo.screeenerRequestMapClient();
        //screenerDemo.screeenerRequestObjectClient();
    }

    private void screeenerRequest1() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .gt(ScreenerField.LEVEREDFREECASHFLOW1YRGROWTH, 2)
            .gt(ScreenerField.NETINCOME1YRGROWTH, 5)
            .build();

        YahooResponse resp = client.execute(req);
        String json = resp.getJson();

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void screeenerRequestTotalOnly() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .gt(ScreenerField.EODPRICE, 0.01)
            .setTotalOnly(true)
            .build();

        YahooResponse resp = client.execute(req);
        String json = resp.getJson();

        System.out.println("--JSON RESPONSE--");
        System.out.println(json);
    }

    private void screeenerRequest2() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .setSize(50)
          //  .enableRequestBatching()
           // .setOffset(90)
            .in(ScreenerField.REGION, Region.UNITED_STATES)
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

        YahooFinanceBatchRequest batchRequest = (YahooFinanceBatchRequest) req;

        YahooBatchResponse resp = client.executeBatch(req);


        List<String> jsonList = resp.getJson();
        String json = jsonList.get(0);

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

    private void screeenerRequestMapClient() throws IOException
    {
        YahooFinanceClient client = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());

        // still _VERY_ beta

        YahooFinanceRequest req = YahooRequestBuilder.api()
            .screener()
            .setSize(100)
            .in(ScreenerField.REGION, Region.UNITED_STATES)
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

        YahooResponse resp = client.execute(req);
        List<Map<String,Object>> listOfMaps = resp.getAsListOfMaps();

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

//    private void screeenerRequestObjectClient() throws IOException
//    {
//        YahooFinanceClient baseClient = new YahooFinanceClient(HttpClientAdapterFactory.createDefaultOkHttpClient());
//        YahooFinanceObjectClient client = new YahooFinanceObjectClient(baseClient);
//
//        // still _VERY_ beta
//
//        YahooFinanceRequest req = YahooRequestBuilder.api()
//            .screener()
//            .setSize(100)
//            .in(ScreenerField.REGION, Region.UNITED_STATES)
//            .lt(ScreenerField.PERATIO, 22)
//            .lt(ScreenerField.PRICEBOOKRATIO, 3.5)
//            .gt(ScreenerField.ALTMANZSCOREUSINGTHEAVERAGESTOCKINFORMATIONFORAPERIOD, 3)
//            .lt(ScreenerField.TOTALDEBTEQUITY, 110)
//            .gt(ScreenerField.CURRENTRATIO, 1.5)
//            .gt(ScreenerField.RETURNONEQUITY, 3)
//            .gt(ScreenerField.NETINCOMEMARGIN, 1)
//            .lt(ScreenerField.PEGRATIO_5Y, 1.1)
//            .gt(ScreenerField.EODPRICE, 0.3)
//            .build();
//
//        //ScreenerResult[] arrayResult = client.executeRequest(req, ScreenerResult[].class);
//        //List<ScreenerResult> listResult = client.fetchObjects(req, ScreenerResult.class);
//
//    }


}
