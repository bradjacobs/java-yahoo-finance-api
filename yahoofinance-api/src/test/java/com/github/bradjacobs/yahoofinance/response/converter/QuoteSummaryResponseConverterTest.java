package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.util.JsonConverter;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import static org.testng.Assert.*;

public class QuoteSummaryResponseConverterTest
{
    private static final QuoteSummaryResponseConverter converter = new QuoteSummaryResponseConverter();

    @Test
    public void testConvertMultiModulesToMap() throws Exception
    {
        List<String> expectedModules =
                Arrays.asList("cashflowStatementHistory", "balanceSheetHistory",
                        "incomeStatementHistory","incomeStatementHistoryQuarterly",
                        "financialData");

        String responseJson = ResourceUtil.readResourceFileAsString("msft_quote_summary_multi_modules.json");
        Map<String, Map<String, Object>> moduleMaps = converter.convertToMapOfMaps(responseJson);

        // must refer to the test json input to determine 'expected results'
        assertNotNull(moduleMaps);
        assertEquals(moduleMaps.size(), expectedModules.size());

        for (String expectedModule : expectedModules) {
            assertTrue(moduleMaps.containsKey(expectedModule), "Could not find expected module in map: " + expectedModule);
        }
    }

    @Test
    public void testConvertAllModulesToMap() throws Exception
    {
        String responseJson = ResourceUtil.readResourceFileAsString("msft_quote_summary_all_modules.json");
        Map<String, Map<String, Object>> moduleMaps = converter.convertToMapOfMaps(responseJson);

        // must refer to the test json input to determine 'expected results'
        assertNotNull(moduleMaps);

        Map<String, Object> summaryDetailMap = moduleMaps.get("summaryDetail");
        Map<String, Object> industryTrendMap = moduleMaps.get("industryTrend");
        assertNotNull(summaryDetailMap);
        assertNotNull(industryTrendMap);

        // 'currently' quoteSummary will leave fields w/ null values
        //    check that industryTrendMap does have a peRatio, but with a 'null' value
        assertTrue(industryTrendMap.containsKey("peRatio"));
        Object peRatioValue = industryTrendMap.get("peRatio");
        assertNull(peRatioValue);

        Object volumeObj = summaryDetailMap.get("volume");
        assertNotNull(volumeObj);
        assertEquals(volumeObj.getClass().getSimpleName(), "Long","expected volume value to be of type 'Long'");
        assertEquals(Long.valueOf(volumeObj.toString()).longValue(), 18055730L);

        String pJson = JsonConverter.toPrettyJson(moduleMaps);
        int kjkj = 333;
    }

}
