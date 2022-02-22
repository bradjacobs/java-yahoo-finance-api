package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.model.ChartResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import org.testng.annotations.Test;

import java.util.*;

import static org.testng.Assert.*;

public class DefaultResponsePojoConverterTest
{
    @Test
    public void testConvertToPojoList() throws Exception
    {
        Map<String,Object> objMap1 = new HashMap<>();
        objMap1.put("date", "2020-12-12");
        objMap1.put("close", 123.44d);
        objMap1.put("volume", 12345L);

        Map<String,Object> objMap2 = new HashMap<>();
        objMap2.put("date", "2020-12-13");
        objMap2.put("close", 456.44d);
        objMap2.put("volume", 54321L);

        List<Map<String,Object>> listOfMaps = Arrays.asList(objMap1, objMap2);

        List<ChartResult> expectedResults = createExpectedResults(listOfMaps);

        DefaultResponsePojoConverter converter = new DefaultResponsePojoConverter( new ChartResponseConverter());

        List<ChartResult> chartResultList = converter.convertToListOfPojos(listOfMaps, ChartResult.class);
        assertNotNull(chartResultList);
        assertEquals(chartResultList.size(), 2);
        assertEquals(chartResultList, expectedResults);
    }
    @Test
    public void testConvertToPojoMap() throws Exception
    {
        Map<String,Object> objMap1 = new HashMap<>();
        objMap1.put("date", "2020-12-12");
        objMap1.put("close", 123.45d);
        objMap1.put("open", 123.00d);
        objMap1.put("high", 124.00d);
        objMap1.put("low", 122.01d);
        objMap1.put("adjclose", 123.52d);
        objMap1.put("volume", 12345L);

        Map<String,Object> objMap2 = new HashMap<>();
        objMap2.put("date", "2020-12-13");
        objMap2.put("close", 456.48d);
        objMap2.put("volume", 54321L);

        Map<String,Map<String,Object>> inputMapOfMaps = new LinkedHashMap<>();
        inputMapOfMaps.put((String)objMap1.get("date"), objMap1);
        inputMapOfMaps.put((String)objMap2.get("date"), objMap2);

        Map<String, ChartResult> expectedResults = createExpectedResults(inputMapOfMaps);

        DefaultResponsePojoConverter converter = new DefaultResponsePojoConverter( new ChartResponseConverter());

        Map<String, ChartResult> chartResultList = converter.convertToMapOfPojos(inputMapOfMaps, ChartResult.class);
        assertNotNull(chartResultList);
        assertEquals(chartResultList.size(), 2);
        assertEquals(chartResultList, expectedResults);
    }

    @Test(expectedExceptions = { IllegalArgumentException.class } )
    public void testMissingClass() throws Exception
    {
        Map<String,Object> objMap1 = new HashMap<>();
        objMap1.put("date", "2020-12-12");
        objMap1.put("close", 123.44d);
        objMap1.put("volume", 12345L);
        List<Map<String,Object>> listOfMaps = Collections.singletonList(objMap1);

        DefaultResponsePojoConverter converter = new DefaultResponsePojoConverter( new ChartResponseConverter());
        List<ChartResult> chartResultList = converter.convertToListOfPojos(listOfMaps, null);
    }


    ////

    private Map<String,ChartResult> createExpectedResults(Map<String,Map<String,Object>> inputMapOfMaps)
    {
        if (inputMapOfMaps == null || inputMapOfMaps.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String,ChartResult> resultMap = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, Object>> entry : inputMapOfMaps.entrySet()) {
            Map<String, Object> valueMap = entry.getValue();
            resultMap.put(entry.getKey(), createExpectedResult(valueMap));
        }
        return resultMap;
    }

    private List<ChartResult> createExpectedResults(List<Map<String,Object>> inputListOfMaps)
    {
        if (inputListOfMaps == null || inputListOfMaps.isEmpty()) {
            return Collections.emptyList();
        }

        List<ChartResult> resultList = new ArrayList<>();
        for (Map<String, Object> valueMap : inputListOfMaps) {
            resultList.add( createExpectedResult(valueMap));
        }
        return resultList;
    }

    private ChartResult createExpectedResult(Map<String, Object> valueMap) {
        JsonMapper mapper = JsonMapperSingleton.getInstance();
        try {
            String jsonString = mapper.writeValueAsString(valueMap);
            return mapper.readValue(jsonString, new TypeReference<ChartResult>(){});
        }
        catch (Exception e) {
            throw new IllegalArgumentException("Unable to convert data to ChartResult: " + e.getMessage(), e);
        }
    }
}
