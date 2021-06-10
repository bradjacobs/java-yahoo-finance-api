/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.response;

import org.testng.annotations.Test;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;

public class ChartResponseConverterTest
{
    private static final ChartResponseConverter chartResponseConverter = new ChartResponseConverter();

    // need delta b/c Doubles might not be an 'exact' match
    private static final Double DELTA = 0.00001;


    @Test
    public void testConvertToListOfMaps() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_5d_formatted.json");
        List<Map<String, Object>> listOfMapRecords = chartResponseConverter.convertToListOfMaps(originalJson);

        assertNotNull(listOfMapRecords);
        assertEquals(listOfMapRecords.size(), LIST_SIZE);

        for (int i = 0; i < listOfMapRecords.size(); i++)
        {
            Map<String, Object> entryMap = listOfMapRecords.get(i);
            assertNotNull(entryMap, "list contains a null entry map record!");
            assertEquals(entryMap.get("timestamp"), expectedTimestamps[i], "mismatch of expected timestamp");
            assertEquals(entryMap.get("volume"), expectedVolumes[i], "mismatch of expected volume");

            assertEquals((Double)entryMap.get("open"), expectedOpens[i], DELTA, "mismatch of expected open");
            assertEquals((Double)entryMap.get("high"), expectedHighs[i], DELTA, "mismatch of expected high");
            assertEquals((Double)entryMap.get("low"), expectedLows[i], DELTA, "mismatch of expected low");
            assertEquals((Double)entryMap.get("close"), expectedCloses[i], DELTA, "mismatch of expected close");
            assertEquals((Double)entryMap.get("adjclose"), expectedAdjCloses[i], DELTA, "mismatch of expected adjclose");
        }
    }

    // todo - fix and move to a different location.
//    @Test
//    public void testConvertToPojos() throws Exception
//    {
//        String originalJson = readTestResourceFile("aapl_chart_5d_formatted.json");
//        List<PriceHistoryRecord> recordList = xx
//
//        assertNotNull(recordList);
//        assertEquals(recordList.size(), LIST_SIZE);
//
//        for (int i = 0; i < recordList.size(); i++)
//        {
//            PriceHistoryRecord record = recordList.get(i);
//            assertNotNull(record, "list contains a null record!");
//            assertEquals(record.getTimestamp(), expectedTimestamps[i], "mismatch of expected timestamp");
//            assertEquals(record.getVolume(), expectedVolumes[i], "mismatch of expected volume");
//
//            assertEquals(record.getOpen(), expectedOpens[i], DELTA, "mismatch of expected open");
//            assertEquals(record.getHigh(), expectedHighs[i], DELTA, "mismatch of expected high");
//            assertEquals(record.getLow(), expectedLows[i], DELTA, "mismatch of expected low");
//            assertEquals(record.getClose(), expectedCloses[i], DELTA, "mismatch of expected close");
//            assertEquals(record.getAdjclose(), expectedAdjCloses[i], DELTA, "mismatch of expected adjclose");
//        }
//    }

    // test valid response, but didn't have any prices
    @Test
    public void testNoResultsResponse() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_no_results.json");
        List<Map<String, Object>> listOfMaps = chartResponseConverter.convertToListOfMaps(originalJson);

        assertNotNull(listOfMaps, "expected non null response");
        assertEquals(listOfMaps.size(), 0, "mismatch expected list size");

    }

    // response has timestamps + close only
    @Test
    public void testCloseResponse() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_5d_cl.json");
        List<Map<String, Object>> listOfMaps = chartResponseConverter.convertToListOfMaps(originalJson);

        assertNotNull(listOfMaps);
        assertEquals(listOfMaps.size(), LIST_SIZE);

        for (int i = 0; i < listOfMaps.size(); i++)
        {
            Map<String, Object> entryMap = listOfMaps.get(i);
            assertNotNull(entryMap, "list contains a null entry map record!");
            assertEquals(entryMap.get("timestamp"), expectedTimestamps[i], "mismatch of expected timestamp");

            assertEquals((Double)entryMap.get("close"), expectedCloses[i], DELTA, "mismatch of expected close");

            assertNull(entryMap.get("volume"), "mismatch of expected volume");
            assertNull((Double)entryMap.get("open"), "mismatch of expected open");
            assertNull((Double)entryMap.get("high"), "mismatch of expected high");
            assertNull((Double)entryMap.get("low"), "mismatch of expected low");
            assertNull((Double)entryMap.get("adjclose"), "mismatch of expected adjclose");
        }
    }


    // response has timestamps + close + adjclose only
    @Test
    public void testCloseAdjCloseResponse() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_5d_cl_adjcl.json");
        List<Map<String, Object>> listOfMaps = chartResponseConverter.convertToListOfMaps(originalJson);

        assertNotNull(listOfMaps);
        assertEquals(listOfMaps.size(), LIST_SIZE);

        for (int i = 0; i < listOfMaps.size(); i++)
        {
            Map<String, Object> entryMap = listOfMaps.get(i);
            assertNotNull(entryMap, "list contains a null entry map record!");
            assertEquals(entryMap.get("timestamp"), expectedTimestamps[i], "mismatch of expected timestamp");

            assertEquals((Double)entryMap.get("close"), expectedCloses[i], DELTA, "mismatch of expected close");
            assertEquals((Double)entryMap.get("adjclose"), expectedAdjCloses[i], DELTA, "mismatch of expected adjclose");

            assertNull(entryMap.get("volume"), "mismatch of expected volume");
            assertNull((Double)entryMap.get("open"), "mismatch of expected open");
            assertNull((Double)entryMap.get("high"), "mismatch of expected high");
            assertNull((Double)entryMap.get("low"), "mismatch of expected low");
        }
    }


    // response had timestamps suppressed
    @Test(expectedExceptions = { IllegalStateException.class },
        expectedExceptionsMessageRegExp = "Cannot convert price history to map: Timestamps missing.")
    public void testMissingTimestamps() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_5d_no_ts.json");
        List<Map<String, Object>> listOfMaps = chartResponseConverter.convertToListOfMaps(originalJson);
    }


    /////


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


    private static final Long[] expectedTimestamps = {1618839000L, 1618925400L, 1619011800L, 1619098200L, 1619184600L};
    private static final Long[] expectedVolumes = {94264200L, 94812300L, 68847100L, 84566500L, 78657500L};
    private static final Double[] expectedHighs = {135.47000122070312d, 135.52999877929688d, 133.75d, 134.14999389648438d, 135.1199951171875d};
    private static final Double[] expectedLows = {133.33999633789062d, 131.80999755859375d, 131.3000030517578d, 131.41000366210938d, 132.16000366210938d};
    private static final Double[] expectedOpens = {133.50999450683594d, 135.02000427246094d, 132.36000061035156d, 133.0399932861328d, 132.16000366210938d};
    private static final Double[] expectedCloses = {134.83999633789062d, 133.11000061035156d, 133.5d, 131.94000244140625d, 134.32000732421875d};
    private static final Double[] expectedAdjCloses = {134.73999633789062d, 133.01000061035156d, 133.4d, 131.84000244140625d, 134.22000732421875d};


    private static final int LIST_SIZE = expectedAdjCloses.length;

}
