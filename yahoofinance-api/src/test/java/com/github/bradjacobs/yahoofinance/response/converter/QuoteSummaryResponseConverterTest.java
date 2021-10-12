package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
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
    public void testConvertToMap() throws Exception
    {
        List<String> expectedModules =
                Arrays.asList("cashflowStatementHistory", "balanceSheetHistory",
                        "incomeStatementHistory","incomeStatementHistoryQuarterly",
                        "financialData");


//        String responseJson = ResourceUtil.readResourceFileAsString("msft_quote_summary_multi_modules.json");
        String responseJson = ResourceUtil.readResourceFileAsString("msft_quote_summary_all_modules.json");
        Map<String, Map<String, Object>> moduleMaps = converter.convertToMapOfMaps(responseJson);

        // must refer to the test json input to determine 'expected results'
        assertNotNull(moduleMaps);
        //assertEquals(moduleMaps.size(), expectedModules.size());

        for (String expectedModule : expectedModules) {
            assertTrue(moduleMaps.containsKey(expectedModule), "Could not find expected module in map: " + expectedModule);
        }

        JsonMapper prettyMapper = JsonMapperSingleton.getPrettyInstance();

        String pJson = prettyMapper.writeValueAsString(moduleMaps);


        // "cashflowStatementHistory" -> {LinkedHashMap@1909}  size = 2
        //"balanceSheetHistory" -> {LinkedHashMap@1911}  size = 2
        //"incomeStatementHistory" -> {LinkedHashMap@1913}  size = 2
        //"financialData" -> {LinkedHashMap@1915}  size = 30

        int kddjk = 333;

    }

}
