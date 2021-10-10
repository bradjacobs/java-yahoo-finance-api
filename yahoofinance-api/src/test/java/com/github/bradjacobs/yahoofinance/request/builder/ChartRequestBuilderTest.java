package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Map;

import static com.github.bradjacobs.yahoofinance.request.builder.ParamKeys.INCLUDE_TIMESTAMPS;
import static org.testng.Assert.*;

public class ChartRequestBuilderTest
{
    private ChartRequestBuilder chartRequestBuilder;

    @BeforeTest
    private void setUp() {
        this.chartRequestBuilder = new ChartRequestBuilder();
    }

    @Test
    public void testBaseCase() throws Exception {

        String tickerSymbol = "MSFT";
        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker(tickerSymbol)
            .withRange(Range.FIVE_DAYS)
            .withInterval(Interval.ONE_DAY)
            .build();

        assertNotNull(request, "Expected non null request from builder");

        assertEquals(request.getTicker(), tickerSymbol, "Mismatch expected ticker value");

        Map<String, String> paramMap = getParamMap(request);

        assertEquals(paramMap.get("range"), "5d");
        assertEquals(paramMap.get("interval"), "1d");
    }

    @Test
    public void testStartEndParams() throws Exception {

        Long period1 = 1612137600L;
        Long period2 = 1619827200L;

        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker("AAPL")
            .setStart(period1)
            .setEnd(period2)
            .build();

        Map<String, String> paramMap = getParamMap(request);

        assertEquals(paramMap.get("period1"), period1.toString());
        assertEquals(paramMap.get("period2"), period2.toString());
    }
    @Test
    public void testStartEndParamAlternativeSet() throws Exception {

        Long period1 = 1612137600L;
        Long period2 = 1619827200L;

        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker("AAPL")
            .setTimeRange(period1, period2)
            .build();

        Map<String, String> paramMap = getParamMap(request);

        assertEquals(paramMap.get("period1"), period1.toString());
        assertEquals(paramMap.get("period2"), period2.toString());
    }

    @Test
    public void testStartEndParamsWithMillis() throws Exception {

        Long period1 = 1612137600987L;
        Long period2 = 1619827200555L;
        Long expectedPeriod1 = period1 / 1000;
        Long expectedPeriod2 = period2 / 1000;

        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker("AAPL")
            .setStart(period1)
            .setEnd(period2)
            .build();

        Map<String, String> paramMap = getParamMap(request);

        assertEquals(paramMap.get("period1"), expectedPeriod1.toString());
        assertEquals(paramMap.get("period2"), expectedPeriod2.toString());
    }

    // if both period1= period2= __AND__ range= are both set, then
    //  period1+period2 wins
    @Test
    public void testStartEndOverrideRange() throws Exception {

        Long period1 = 1612137600L;
        Long period2 = 1619827200L;

        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker("AAPL")
            .withRange(Range.FIVE_DAYS)
            .setStart(period1)
            .setEnd(period2)
            .build();

        Map<String, String> paramMap = getParamMap(request);

        assertNull(paramMap.get("range"));
        assertEquals(paramMap.get("period1"), period1.toString());
        assertEquals(paramMap.get("period2"), period2.toString());
    }


    @DataProvider(name = "event-permutations")
    public Object[][] invalidTimeZoneParams(){
        return new Object[][] {
                // includeDiv, includeSplit, expectedString
                {true, true, "div,split"},
                {true, false, "div"},
                {true, null, "div"},
                {null, true, "split"},
                {false, false, null},
        };
    }

    @Test(dataProvider = "event-permutations")
    public void testSetEvents(Boolean includeDividends, Boolean includeSplits, String expectedEventString) throws Exception {

        YahooFinanceRequest request = this.chartRequestBuilder
            .withTicker("AAPL")
            .withRange(Range.FIVE_DAYS)
            .withDividends(includeDividends)
            .withSplits(includeSplits)
            .build();

        Map<String, String> paramMap = getParamMap(request);
        assertEquals(paramMap.get("events"), expectedEventString);
    }


    @Test
    public void testIndicatorFieldSelection() throws Exception {

        this.chartRequestBuilder
            .withTicker("AAPL")
            .withRange(Range.FIVE_DAYS)
            .withIndicatorCloseAdjCloseOnly();

        YahooFinanceRequest req = chartRequestBuilder.build();
        Map<String, String> paramMap = getParamMap(req);
        assertEquals(paramMap.get("indicators"), "close");
        assertEquals(paramMap.get("includeAdjustedClose"), "true");

        chartRequestBuilder = chartRequestBuilder.withIndicatorCloseOnly();
        req = chartRequestBuilder.build();
        paramMap = getParamMap(req);
        assertEquals(paramMap.get("indicators"), "close");
        assertEquals(paramMap.get("includeAdjustedClose"), "false");

        chartRequestBuilder = chartRequestBuilder.withIndicatorAdjCloseOnly();
        req = chartRequestBuilder.build();
        paramMap = getParamMap(req);
        assertEquals(paramMap.get("indicators"), "adjclose");
        assertEquals(paramMap.get("includeAdjustedClose"), "true");

        chartRequestBuilder = chartRequestBuilder.withIndicatorAllFields();
        req = chartRequestBuilder.build();
        paramMap = getParamMap(req);
        assertNull(paramMap.get("indicators"));
        assertEquals(paramMap.get("includeAdjustedClose"), "true");
    }


    @Test
    public void testIncludeTimestampsParam() throws Exception {

        YahooFinanceRequest request = this.chartRequestBuilder
                .withTicker("AAPL")
                .withRange(Range.FIVE_DAYS)
                .withTimestamps(true)
                .build();

        Map<String, String> paramMap = getParamMap(request);
        assertEquals(paramMap.get(INCLUDE_TIMESTAMPS), "true");

        request = this.chartRequestBuilder
                .withTimestamps(false)
                .build();
        paramMap = getParamMap(request);
        assertEquals(paramMap.get(INCLUDE_TIMESTAMPS), "false");

        request = this.chartRequestBuilder
                .withTimestamps(null)
                .build();
        paramMap = getParamMap(request);
        assertNull(paramMap.get(INCLUDE_TIMESTAMPS));
    }



    // helper to get the paramMap w/ some asserts
    private Map<String, String> getParamMap(YahooFinanceRequest request)
    {
        assertNotNull(request, "Expected non null request");
        Map<String, String> paramMap = request.getParamMap();
        assertNotNull(paramMap, "Expected non null paramMap from request");
        assertTrue(paramMap.size() > 0, "Expected non empty paramMap from request");

        return paramMap;
    }

    @Test
    public void testRangeOverrideStartEnd() throws Exception {

        // todo
    }
}