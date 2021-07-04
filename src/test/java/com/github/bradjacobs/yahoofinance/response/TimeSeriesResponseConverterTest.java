package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.testng.Assert.*;


public class TimeSeriesResponseConverterTest
{

    @Test
    public void testConvertToListOfMaps() throws Exception
    {
        String originalJson = ResourceUtil.readResourceFileAsString("aapl_timeseries_response.json");

        TimeSeriesResponseConverter timeseriesResponseConverter = new TimeSeriesResponseConverter(true, true);

        Map<String, Map<String, Object>> mapOfMaps = timeseriesResponseConverter.convertToMapOfMaps(originalJson);

        assertNotNull(mapOfMaps, "expected non-null map result");
        assertEquals(mapOfMaps.size(), 3);

        Map<String, Object> annualValueMap = mapOfMaps.get("annual");
        assertNotNull(annualValueMap, "expected non-null map result");
        assertTrue(annualValueMap.containsKey("2017-09-30"));
        assertTrue(annualValueMap.containsKey("2018-09-30"));
        assertTrue(annualValueMap.containsKey("2019-09-30"));

        // todo -- this is (obviously) only partially done.

    }

}
