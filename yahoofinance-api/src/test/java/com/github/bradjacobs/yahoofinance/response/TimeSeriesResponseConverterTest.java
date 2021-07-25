package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.converter.TimeSeriesResponseConverter;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.Map;

import static org.testng.Assert.*;


public class TimeSeriesResponseConverterTest
{

    @Test
    public void testConvertAnnualToMapOfMaps() throws Exception
    {
        String originalJson = ResourceUtil.readResourceFileAsString("aapl_timeseries_response.json");

        TimeSeriesResponseConverter timeseriesResponseConverter = new TimeSeriesResponseConverter();

        Map<String, Map<String, Object>> annualValueMap = timeseriesResponseConverter.convertToMapOfMaps(originalJson);

        assertNotNull(annualValueMap, "expected non-null map result");
        assertEquals(annualValueMap.size(), 3);

        assertTrue(annualValueMap.containsKey("2017-09-30"));
        assertTrue(annualValueMap.containsKey("2018-09-30"));
        assertTrue(annualValueMap.containsKey("2019-09-30"));

        // TODO -- this is (obviously) only partially done.

    }

}
