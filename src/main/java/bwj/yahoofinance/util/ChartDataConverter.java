/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import bwj.yahoofinance.objects.PriceHistoryRecord;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Given a JSON response from a chart endpoint request (i.e.  v8/finance/chart/{symbol} ),
 *  see if can convert the data into a more friendly format.
 *
 *  Note: this is a still a prototype to see how conversion might work
 *     still needs error case handling.
 */
public class ChartDataConverter
{
    private static final ObjectMapper mapper = new ObjectMapper();

    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_OPEN = "open";
    private static final String KEY_LOW = "low";
    private static final String KEY_HIGH = "high";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_VOLUME = "volume";
    private static final String KEY_ADJ_CLOSE = "adjclose";

    public List<Map<String, Number>> toListOfMaps(String json) throws Exception
    {
        JsonNode root = null;
        try {
            root = mapper.readTree(json);
        }
        catch (Exception e) {
            throw new RuntimeException("Unable to parse json content: " + e.getMessage(), e);
        }

        //JsonNode innerArrayElement = root.at("/chart/result/0");
        JsonNode innerArrayElement = root.required("chart").required("result").required(0);

        JsonNode indicatorsNode = innerArrayElement.path("indicators");
        JsonNode quoteValuesNode = indicatorsNode.path("quote").path(0);
        JsonNode adjQuoteValuesNode = indicatorsNode.path("adjclose").path(0);

        Double[] openValues = mapper.convertValue(quoteValuesNode.path(KEY_OPEN), Double[].class);
        Double[] closeValues = mapper.convertValue(quoteValuesNode.path(KEY_CLOSE), Double[].class);
        Double[] lowValues = mapper.convertValue(quoteValuesNode.path(KEY_LOW), Double[].class);
        Double[] highValues = mapper.convertValue(quoteValuesNode.path(KEY_HIGH), Double[].class);
        Long[] volumeValues = mapper.convertValue(quoteValuesNode.path(KEY_VOLUME), Long[].class);

        Double[] adjCloseValues = mapper.convertValue(adjQuoteValuesNode.path(KEY_ADJ_CLOSE), Double[].class);

        Long[] timestampValues = mapper.convertValue(innerArrayElement.path(KEY_TIMESTAMP), Long[].class);

        List<Map<String, Number>> resultKeyValueList = new ArrayList<>();

        // ASSERT all lists are same length
        int entryCount = timestampValues.length;

        for (int i = 0; i < entryCount; i++)
        {
            Map<String,Number> entryMap = new HashMap<>();
            entryMap.put(KEY_TIMESTAMP, timestampValues[i]);
            entryMap.put(KEY_OPEN, openValues[i]);
            entryMap.put(KEY_CLOSE, closeValues[i]);
            entryMap.put(KEY_LOW, lowValues[i]);
            entryMap.put(KEY_HIGH, highValues[i]);
            entryMap.put(KEY_VOLUME, volumeValues[i]);
            entryMap.put(KEY_ADJ_CLOSE, adjCloseValues[i]);
            resultKeyValueList.add(entryMap);
        }

        return resultKeyValueList;
    }


    public List<PriceHistoryRecord> toRecordList(String json) throws Exception
    {
        List<Map<String, Number>> listOfMapRecords = toListOfMaps(json);
        PriceHistoryRecord[] recordArray = mapper.convertValue(listOfMapRecords, PriceHistoryRecord[].class);
        return Arrays.asList(recordArray);
    }

}
