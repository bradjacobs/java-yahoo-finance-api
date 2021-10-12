/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.response.converter.util;

import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.List;

import static org.testng.Assert.*;

public class JsonNestedFormatRemoverTest
{
    @Test
    public void testFlattenRawValueHappyPath() throws Exception
    {
        String originalJson = ResourceUtil.readResourceFileAsString("appl_balhistory_formated.json");

        String expectedSubstring1 = "\"cash\":38016000000";  // from first balSheet
        String expectedSubstring2 = "\"shortTermInvestments\":51713000000";  // from second balSheet

        JsonNestedFormatRemover formatRemover = new JsonNestedFormatRemover(true);
        String updatedJson = formatRemover.removeFormats(originalJson);

        // assertions currently require knowledge of data contents within file.
        assertNotNull(updatedJson, "expeted a non-null value from JsonNestedFormatRemover.");
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring1);
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring2);
    }

    @Test
    public void testNestedDateArray() throws Exception
    {
        String originalJson = ResourceUtil.readResourceFileAsString("msft_quote_summary_calendar_events.json");

        JsonNestedFormatRemover formatRemover = new JsonNestedFormatRemover(true);
        String updatedJson = formatRemover.removeFormats(originalJson);

        assertNotNull(updatedJson, "expeted a non-null value from JsonNestedFormatRemover.");
        TestJsonPathCollectionConverter jsonConverter
                = new TestJsonPathCollectionConverter();

        List<Object> dateList = jsonConverter.extractListOfObjects(updatedJson, "$.quoteSummary.result[0].calendarEvents.earnings.earningsDate[*]");
        assertNotNull(dateList);
        assertEquals(dateList.size(), 2);

        assertTrue(dateList.get(0) instanceof Long);
        assertTrue(dateList.get(1) instanceof Long);
        assertEquals((Long)dateList.get(0), Long.valueOf(1635159540));
        assertEquals((Long)dateList.get(1), Long.valueOf(1635508800));
    }

}
