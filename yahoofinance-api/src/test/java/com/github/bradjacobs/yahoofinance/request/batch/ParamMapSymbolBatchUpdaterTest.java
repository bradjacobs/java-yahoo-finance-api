package com.github.bradjacobs.yahoofinance.request.batch;

import org.testng.annotations.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.*;

public class ParamMapSymbolBatchUpdaterTest {

    private static final String BASE_CASE_SYMBOL_KEY = "symbols";
    private static final String BASE_CASE_SYMBOL_VALUES = "AA,BB,CC,DD,EE,FF,GG,HH";

    private Map<String,String> createTestMap(String symbolString) {
        return new LinkedHashMap<String,String>() {{
            put("key_A", "value_A");
            put(BASE_CASE_SYMBOL_KEY, symbolString);
            put("key_C", "value_C");
        }};
    }


    @Test
    public void testBaseCase() throws Exception {
        Map<String, String> inputMap = createTestMap(BASE_CASE_SYMBOL_VALUES);
        int batchSize = 3;

        Map<String, String> expectedBatchMap1 = createTestMap("AA,BB,CC");
        Map<String, String> expectedBatchMap2 = createTestMap("DD,EE,FF");
        Map<String, String> expectedBatchMap3 = createTestMap("GG,HH");

        ParamMapSymbolBatchUpdater paramMapSymbolBatchUpdater =
                new ParamMapSymbolBatchUpdater(BASE_CASE_SYMBOL_KEY, batchSize);

        Map<String, String> batchMap1 = paramMapSymbolBatchUpdater.convert(inputMap, 1);
        Map<String, String> batchMap2 = paramMapSymbolBatchUpdater.convert(inputMap, 2);
        Map<String, String> batchMap3 = paramMapSymbolBatchUpdater.convert(inputMap, 3);

        assertEqualsDeep(batchMap1, expectedBatchMap1);
        assertEqualsDeep(batchMap2, expectedBatchMap2);
        assertEqualsDeep(batchMap3, expectedBatchMap3);
    }


    @Test
    public void testEvenBatchDivisor() throws Exception {
        Map<String, String> inputMap = createTestMap(BASE_CASE_SYMBOL_VALUES);
        int batchSize = 4;

        Map<String, String> expectedBatchMap1 = createTestMap("AA,BB,CC,DD");
        Map<String, String> expectedBatchMap2 = createTestMap("EE,FF,GG,HH");

        ParamMapSymbolBatchUpdater paramMapSymbolBatchUpdater =
                new ParamMapSymbolBatchUpdater(BASE_CASE_SYMBOL_KEY, batchSize);

        Map<String, String> batchMap1 = paramMapSymbolBatchUpdater.convert(inputMap, 1);
        Map<String, String> batchMap2 = paramMapSymbolBatchUpdater.convert(inputMap, 2);

        assertEqualsDeep(batchMap1, expectedBatchMap1);
        assertEqualsDeep(batchMap2, expectedBatchMap2);
    }


    @Test
    public void testOversizeBatchSize() throws Exception {
        Map<String, String> inputMap = createTestMap(BASE_CASE_SYMBOL_VALUES);
        int batchSize = 99;

        Map<String, String> expectedBatchMap1 = createTestMap(BASE_CASE_SYMBOL_VALUES);

        ParamMapSymbolBatchUpdater paramMapSymbolBatchUpdater =
                new ParamMapSymbolBatchUpdater(BASE_CASE_SYMBOL_KEY, batchSize);

        Map<String, String> batchMap1a = paramMapSymbolBatchUpdater.convert(inputMap, 1);
        Map<String, String> batchMap1b = paramMapSymbolBatchUpdater.convert(inputMap, 55);

        assertEqualsDeep(batchMap1a, expectedBatchMap1);
        assertEqualsDeep(batchMap1b, expectedBatchMap1);
    }


    @Test
    public void testSingleTickerValue() throws Exception {
        Map<String, String> inputMap = createTestMap("ABC");
        Map<String, String> expectedBatchMap1 = createTestMap("ABC");
        int batchSize = 1;

        ParamMapSymbolBatchUpdater paramMapSymbolBatchUpdater =
                new ParamMapSymbolBatchUpdater(BASE_CASE_SYMBOL_KEY, batchSize);

        Map<String, String> batchMap1 = paramMapSymbolBatchUpdater.convert(inputMap, batchSize);

        assertEqualsDeep(batchMap1, expectedBatchMap1);
    }

}