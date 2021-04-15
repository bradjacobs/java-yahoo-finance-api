/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import org.testng.annotations.Test;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.testng.Assert.*;

public class JsonUtilTest
{
    @Test
    public void testFlattenRawValueHappyPath() throws Exception
    {
        String originalJson = readTestResourceFile("appl_balhistory_formated.json");

        String expectedSubstring1 = "\"cash\" : 38016000000";  // from first balSheet
        String expectedSubstring2 = "\"shortTermInvestments\" : 51713000000";  // from second balSheet

        String updatedJson = JsonUtil.removeRawValues(originalJson);

        // System.out.println(updatedJson);

        // assertions currently require knowledge of data contents within file.
        assertNotNull(updatedJson, "expeted a non-null value from JsonUtil method.");
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring1);
        assertTrue(updatedJson.contains(expectedSubstring1), "Expected substring not found with JSON response: " + expectedSubstring2);
    }



    private String readTestResourceFile(String fileName)
    {
        try {
            URL resource = getClass().getClassLoader().getResource(fileName);
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read test resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }

}
