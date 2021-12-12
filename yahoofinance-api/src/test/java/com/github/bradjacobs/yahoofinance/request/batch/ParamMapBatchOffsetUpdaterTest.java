package com.github.bradjacobs.yahoofinance.request.batch;

import org.testng.annotations.Test;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.testng.Assert.*;

public class ParamMapBatchOffsetUpdaterTest
{
    private static final String BATCH_KEY = "startIndex";

    private Map<String,String> createTestMap(Integer val) {
        return new LinkedHashMap<String,String>() {{
            put("key_A", "value_A");
            put(BATCH_KEY, val.toString());
            put("key_C", "value_C");
        }};
    }

    @Test
    public void testBaseCase() throws Exception
    {
        int batchSize = 5;
        Map<String, String> inputMap = createTestMap(0);

        Map<String, String> expectedBatchMap1 = createTestMap(0);
        Map<String, String> expectedBatchMap2 = createTestMap(5);
        Map<String, String> expectedBatchMap3 = createTestMap(10);

        ParamMapBatchOffsetUpdater paramMapSymbolBatchUpdater =
                new ParamMapBatchOffsetUpdater(BATCH_KEY, batchSize);

        Map<String, String> batchMap1 = paramMapSymbolBatchUpdater.convert(inputMap, 1);
        Map<String, String> batchMap2 = paramMapSymbolBatchUpdater.convert(inputMap, 2);
        Map<String, String> batchMap3 = paramMapSymbolBatchUpdater.convert(inputMap, 3);

        assertEqualsDeep(batchMap1, expectedBatchMap1);
        assertEqualsDeep(batchMap2, expectedBatchMap2);
        assertEqualsDeep(batchMap3, expectedBatchMap3);
    }


    @Test(expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "Must supply a populated paramMap.")
    public void testEmptyParamMapA() throws Exception
    {
        ParamMapBatchOffsetUpdater paramMapSymbolBatchUpdater =
                new ParamMapBatchOffsetUpdater(BATCH_KEY, 10);
        paramMapSymbolBatchUpdater.convert(null, 5);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "Must supply a populated paramMap.")
    public void testEmptyParamMapB() throws Exception
    {
        ParamMapBatchOffsetUpdater paramMapSymbolBatchUpdater =
                new ParamMapBatchOffsetUpdater(BATCH_KEY, 10);
        paramMapSymbolBatchUpdater.convert(Collections.emptyMap(), 5);
    }

}
