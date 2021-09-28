/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.response.converter.util;

import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

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
        assertNotNull(updatedJson, "expeted a non-null value from JsonFormatRemover.");
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring1);
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring2);
    }

}
