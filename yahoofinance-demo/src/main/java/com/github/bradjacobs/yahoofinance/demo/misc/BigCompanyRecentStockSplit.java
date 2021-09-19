package com.github.bradjacobs.yahoofinance.demo.misc;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.demo.misc.objects.KeyStatistics;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.QuoteSummaryBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.types.Exchange;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.io.IOException;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.github.bradjacobs.yahoofinance.types.YahooModule.DEFAULT_KEY_STATISTICS;


/**
 * SCENARIO:
 *
 *   Find the top 50 biggest stocks (via marketcap) in the NYSE/NASDAQ,
 *     Then find out which of these had most recent stock split
 *
 *     TODO -- need to update example... KeyStatistics no longer returning split date
 *
 */
public class BigCompanyRecentStockSplit
{
    public static void main(String[] args) throws IOException
    {
        try {
            doLastSplitInfo();
        }
        catch (Exception e)
        {
            // if something goes totally wrong then write minimal info to std out
            System.out.println("ERROR: " + e.getMessage());
            e.printStackTrace();
        }
    }


    private static void doLastSplitInfo() throws Exception
    {
        YahooFinanceClient client = new YahooFinanceClient();

        // query for the top 50 stocks in the Nasdaq/Nyse
        YahooFinanceRequest screenerRequest = YahooRequestBuilder.api()
            .screener()
            .in(ScreenerField.REGION, Region.UNITED_STATES)
            .in(ScreenerField.EXCHANGE, Exchange.NASDAQ, Exchange.NASDAQGS, Exchange.NASDAQGM, Exchange.NASDAQCM, Exchange.NYSE)
            .setSize(50)
            .setSortDescending(ScreenerField.INTRADAYMARKETCAP)
            .build();

        // execute the screener request.
        YahooResponse screenerResponse = client.execute(screenerRequest);

        // really only want the ticker values, so convert result to map, then grab the map 'keys', which will be the tickers.
        Map<String, Map<String, Object>> screenerMapOfMaps = screenerResponse.getAsMapOfMaps();

        List<String> tickerList = new ArrayList<>(screenerMapOfMaps.keySet());

        // builder to make requests for the quoteSummary endpoint
        QuoteSummaryBuilder quoteSummaryRequestBuilder =
            YahooRequestBuilder.api()
                .quoteSummary()
                .withModules(DEFAULT_KEY_STATISTICS);

        // going to query all the tickers and temporarily stuff all results in a map
        //    (do NOT do this with lots of tickers and blow up the memory)
        Map<String, YahooResponse> tickerQuoteSummaryResponseMap = new HashMap<>();

        int counter = 0;
        for (String ticker : tickerList)
        {
            System.out.println("Fetching for Ticker: " + ticker + "  COUNTER: " + (++counter));
            try
            {
                YahooFinanceRequest quoteSummaryRequest = quoteSummaryRequestBuilder.withTicker(ticker).build();

                YahooResponse resp = client.execute(quoteSummaryRequest);
                tickerQuoteSummaryResponseMap.put(ticker, resp);
            }
            catch (Exception e) {

                // if something goes wrong, then just ignore and continue.

                //  NOTE ---  it __IS__ possible to get a 404
                //    (i.e. yahoo can return a value from screener that does not have quoteSummary  (most likely 'preferred' -P type ticker)
                System.out.println("failed to get quoteSummary for ticker: " + ticker + "  Error: " + e.getMessage());
            }
        }

        // list that will hold all the 'defaultKeyStatistics' key/value pairs for each ticker.
        List<Map<String, Object>> defaultKeyStatisticsList = new ArrayList<>();


        for (Map.Entry<String, YahooResponse> entry : tickerQuoteSummaryResponseMap.entrySet())
        {
            String ticker = entry.getKey();
            YahooResponse resp = entry.getValue();

            // for 'mapOfMaps' on quoteSummary, the keys of the map will be the name of the module
            Map<String, Map<String, Object>> mapOfMaps = resp.getAsMapOfMaps();

            Map<String, Object> defaultKeyStatsMap = mapOfMaps.get("defaultKeyStatistics");

            // stick on the ticker to the key/value map (b/c it will be useful later)
            defaultKeyStatsMap.put("ticker", ticker);

            // add to the list.
            defaultKeyStatisticsList.add(defaultKeyStatsMap);
        }

        // use a JsonMapper as a convenient way to convert the data to pojos
        JsonMapper mapper = JsonMapperSingleton.getInstance();

        // convert the list of 'key/value maps' to a list of 'KeyStatistics' objects
        //   the class 'KeyStatistics' is just a simple made up pojo class.
        List<KeyStatistics> keyStatisticsList = mapper.convertValue(defaultKeyStatisticsList, new TypeReference<List<KeyStatistics>>() {});

        // sort the list with special Comparator, so that the stocks with the most recent stock splits come first.
        keyStatisticsList.sort(new KeyStatisticsLastSplitComparator());

        for (KeyStatistics keyStatistics : keyStatisticsList)
        {
            String ticker = keyStatistics.getTicker();
            Long timestamp = keyStatistics.getLastSplitDate();

            String dateString = "";
            if (timestamp != null) {
                // lazy-ish way to make the timestamp into human-readable string
                dateString = Instant.ofEpochSecond(timestamp)
                    .atZone(ZoneId.systemDefault()).toLocalDate().toString();
            }

            String lastSplitFactor = keyStatistics.getLastSplitFactor();

            // print out the ticker w/ the last Split Date
            System.out.println(String.format("Ticker: %-6s  LastSplitDate: %-10s  LastSplitFactor: %s", ticker, dateString, lastSplitFactor));
        }
    }


    /*
        -- EXAMPLE OUTPUT --

            Ticker: AAPL    LastSplitDate: 2020-08-30  LastSplitFactor: 4:1
            Ticker: TSLA    LastSplitDate: 2020-08-30  LastSplitFactor: 5:1
            Ticker: CMCSA   LastSplitDate: 2017-02-20  LastSplitFactor: 2:1
            Ticker: NKE     LastSplitDate: 2015-12-23  LastSplitFactor: 2:1
            Ticker: NFLX    LastSplitDate: 2015-07-14  LastSplitFactor: 7:1
            Ticker: GOOG    LastSplitDate: 2015-04-26  LastSplitFactor: 10000000:10000000
            Ticker: V       LastSplitDate: 2015-03-18  LastSplitFactor: 4:1
            Ticker: GOOGL   LastSplitDate: 2014-04-02  LastSplitFactor: 1998:1000
            Ticker: MA      LastSplitDate: 2014-01-21  LastSplitFactor: 10:1
            Ticker: ASML    LastSplitDate: 2012-11-28  LastSplitFactor: 77:100
            ...
     */



    /**
     * Custom sort that will sort on the lastSplitDate where the __NEWEST__ date comes first.
     *   Any records that do not have a lastSplit date (i.e. null) will show up last.
     */
    public static class KeyStatisticsLastSplitComparator implements Comparator<KeyStatistics>
    {
        @Override
        public int compare(KeyStatistics o1, KeyStatistics o2)
        {
            if (o1 == o2) {
                return 0;
            }

            Long splitDate1 = o1.getLastSplitDate();
            Long splitDate2 = o2.getLastSplitDate();

            if (splitDate1 == null) {
                if (splitDate2 == null) {
                    return 0;
                }
                else {
                    return 1;
                }
            }
            else if (splitDate2 == null) {
                return -1;
            }
            else {
                return Long.compare(splitDate2, splitDate1);
            }
        }
    }
}
