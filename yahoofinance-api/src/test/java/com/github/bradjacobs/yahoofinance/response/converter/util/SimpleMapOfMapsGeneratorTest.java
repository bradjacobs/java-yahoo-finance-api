package com.github.bradjacobs.yahoofinance.response.converter.util;

import org.testng.annotations.Test;

import java.util.*;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

public class SimpleMapOfMapsGeneratorTest
{
    private static final String DATE_KEY = "date";
    private static final String INT_KEY = "intValue";
    private static final String STRING_KEY = "stringValue";


    @Test
    public void testSortedMapHappyPath() throws Exception
    {
        Map<String, Object> map1 = createTestEntityMap("2021-02-01", 55, "cat");
        Map<String, Object> map2 = createTestEntityMap("2021-01-01", 44, "dog");
        Map<String, Object> map3 = createTestEntityMap("2021-03-01", 77, "cow");

        Map<String, Map<String, Object>> expectedMap = new TreeMap<>();
        expectedMap.put((String)map2.get(DATE_KEY), map2);
        expectedMap.put((String)map1.get(DATE_KEY), new TreeMap<>(map1));  // check that really equals, not jsut identity equals.
        expectedMap.put((String)map3.get(DATE_KEY), map3);

        List<Map<String, Object>> inputList = Arrays.asList(map1, map2, map3);

        SimpleMapOfMapsGenerator mapGenerator = new SimpleMapOfMapsGenerator(DATE_KEY, true);
        Map<String, Map<String, Object>> resultMap = mapGenerator.convertToMap(inputList);
        assertNotNull(resultMap);
        assertEquals(resultMap.size(), 3, "mismatch expected map size");

        List<String> dateKeys = new ArrayList<>(resultMap.keySet());
        assertEquals("2021-01-01", dateKeys.get(0));
        assertEquals("2021-02-01", dateKeys.get(1));
        assertEquals("2021-03-01", dateKeys.get(2));

        assertEquals(expectedMap, resultMap);
    }


    @Test
    public void testInsertOrderMapHappyPath() throws Exception
    {
        Map<String, Object> map1 = createTestEntityMap("2021-02-01", 55, "cat");
        Map<String, Object> map2 = createTestEntityMap("2021-01-01", 44, "dog");
        Map<String, Object> map3 = createTestEntityMap("2021-03-01", 77, "cow");

        Map<String, Map<String, Object>> expectedMap = new LinkedHashMap<>();
        expectedMap.put((String)map1.get(DATE_KEY), map1);
        expectedMap.put((String)map2.get(DATE_KEY), new TreeMap<>(map2));  // check that really equals, not jsut identity equals.
        expectedMap.put((String)map3.get(DATE_KEY), map3);

        List<Map<String, Object>> inputList = Arrays.asList(map1, map2, map3);

        SimpleMapOfMapsGenerator mapGenerator = new SimpleMapOfMapsGenerator(DATE_KEY, false);

        Map<String, Map<String, Object>> resultMap = mapGenerator.convertToMap(inputList);
        assertNotNull(resultMap);
        assertEquals(resultMap.size(), 3, "mismatch expected map size");

        List<String> dateKeys = new ArrayList<>(resultMap.keySet());
        assertEquals("2021-02-01", dateKeys.get(0));
        assertEquals("2021-01-01", dateKeys.get(1));
        assertEquals("2021-03-01", dateKeys.get(2));

        assertEquals(expectedMap, resultMap);
    }





    private  Map<String, Object> createTestEntityMap(String date, int inValue, String strValue)
    {
        Map<String, Object> map = new HashMap<>();
        map.put(DATE_KEY, date);
        map.put(INT_KEY, inValue);
        map.put(STRING_KEY, strValue);
        return map;
    }

}
