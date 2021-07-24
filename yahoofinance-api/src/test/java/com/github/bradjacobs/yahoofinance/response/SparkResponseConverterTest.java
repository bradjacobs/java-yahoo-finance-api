package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

public class SparkResponseConverterTest
{
    // todo - add tests w/ true as well
    private static final SparkResponseConverter sparkResponseConverter = new SparkResponseConverter(false);

    // need delta b/c Doubles might not be an 'exact' match
    private static final Double DELTA = 0.00001;


    // TODO - come back to and fix this test!!!!

    /*
     *   {
     *   "AAPL" : {
     *     "2021-07-02" : 139.96,
     *     "2021-07-06" : 142.02,
     *     "2021-07-07" : 144.57,
     *     ...
     *   }
     * }
     */

    @Test
    public void testConvertMultiTickerMapOfMaps() throws Exception
    {
        Map<String, Map<String, Object>> mapOfMaps = sparkResponseConverter.convertToMapOfMaps(TEST_TWO_TICKER_RESPONSE);

        assertNotNull(mapOfMaps);
        assertEquals(mapOfMaps.size(), 2, "expected map with 2 ticker values");

        Map<String, Object> aaplDataMap = mapOfMaps.get("AAPL");
        Map<String, Object> aalDataMap = mapOfMaps.get("AAL");

        assertNotNull(aaplDataMap);
        assertNotNull(aalDataMap);

        assertEquals(aaplDataMap.size(), 5);
        assertEquals(aalDataMap.size(), 5);

        // todo - fix and finish
    }




    // for the first aapl response
    private static final Long[] expectedTimestamps = {1625232600L, 1625578200L, 1625664600L, 1625751000L, 1625860802L};
    private static final String[] expectedDates = {"2021-07-02", "2021-07-06", "2021-07-07", "2021-07-08", "2021-07-09"};
    private static final Double[] expectedCloses = {139.96d, 142.02d, 144.57d, 143.24d, 145.11d};



    // todo - move below out of hardcoded string mode.

    private static final String TEST_JSON_RESPONSE = "{\n" +
        "  \"AAPL\" : {\n" +
        "    \"timestamp\" : [\n" +
        "      1625232600,\n" +
        "      1625578200,\n" +
        "      1625664600,\n" +
        "      1625751000,\n" +
        "      1625860802\n" +
        "    ],\n" +
        "    \"symbol\" : \"AAPL\",\n" +
        "    \"dataGranularity\" : 300,\n" +
        "    \"close\" : [\n" +
        "      139.96,\n" +
        "      142.02,\n" +
        "      144.57,\n" +
        "      143.24,\n" +
        "      145.11\n" +
        "    ],\n" +
        "    \"end\" : null,\n" +
        "    \"start\" : null,\n" +
        "    \"previousClose\" : null,\n" +
        "    \"chartPreviousClose\" : 137.27\n" +
        "  }\n" +
        "}";

    private static final String TEST_TWO_TICKER_RESPONSE = "{\n" +
        "  \"AAPL\": {\n" +
        "    \"symbol\": \"AAPL\",\n" +
        "    \"previousClose\": null,\n" +
        "    \"chartPreviousClose\": 137.27,\n" +
        "    \"timestamp\": [\n" +
        "      1625232600,\n" +
        "      1625578200,\n" +
        "      1625664600,\n" +
        "      1625751000,\n" +
        "      1625837400\n" +
        "    ],\n" +
        "    \"close\": [\n" +
        "      139.96,\n" +
        "      142.02,\n" +
        "      144.57,\n" +
        "      143.24,\n" +
        "      145.11\n" +
        "    ],\n" +
        "    \"dataGranularity\": 300,\n" +
        "    \"end\": null,\n" +
        "    \"start\": null\n" +
        "  },\n" +
        "  \"AAL\": {\n" +
        "    \"symbol\": \"AAL\",\n" +
        "    \"previousClose\": null,\n" +
        "    \"chartPreviousClose\": 21.51,\n" +
        "    \"timestamp\": [\n" +
        "      1625232600,\n" +
        "      1625578200,\n" +
        "      1625664600,\n" +
        "      1625751000,\n" +
        "      1625837400\n" +
        "    ],\n" +
        "    \"close\": [\n" +
        "      21.48,\n" +
        "      21.01,\n" +
        "      20.31,\n" +
        "      20.35,\n" +
        "      20.89\n" +
        "    ],\n" +
        "    \"dataGranularity\": 300,\n" +
        "    \"end\": null,\n" +
        "    \"start\": null\n" +
        "  }\n" +
        "}";
}
