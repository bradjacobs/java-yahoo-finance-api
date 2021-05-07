/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import bwj.yahoofinance.model.PriceHistoryRecord;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang.ArrayUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Given a JSON response from a chart endpoint request (i.e.  v8/finance/chart/{symbol} ),
 *  see if can convert the data into a more friendly format.
 *
 */
public class ChartDataConverter
{
    // key titles for the output map
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_OPEN = "open";
    private static final String KEY_LOW = "low";
    private static final String KEY_HIGH = "high";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_VOLUME = "volume";
    private static final String KEY_ADJ_CLOSE = "adjclose";


    // path locations for data within the JSON response
    private static final String BASE_PATH = "/chart/result/0";
    private static final String QUOTE_PATH = BASE_PATH + "/indicators/quote/0";
    private static final String ADJ_QUOTE_PATH = BASE_PATH + "/indicators/adjclose/0/" + KEY_ADJ_CLOSE;

    private static final String TIMESTAMP_PATH = BASE_PATH + "/" + KEY_TIMESTAMP;
    private static final String OPEN_PATH = QUOTE_PATH + "/" + KEY_OPEN;
    private static final String LOW_PATH = QUOTE_PATH + "/" + KEY_LOW;
    private static final String HIGH_PATH = QUOTE_PATH + "/" + KEY_HIGH;
    private static final String CLOSE_PATH = QUOTE_PATH + "/" + KEY_CLOSE;
    private static final String VOLUME_PATH = QUOTE_PATH + "/" + KEY_VOLUME;


    private static final boolean EXCEPTION_ON_INVALID_PATH = false;
    private static final ObjectMapper mapper = new ObjectMapper();



    public List<Map<String, Number>> toListOfMaps(String json) throws Exception
    {
        JsonDataExtractor jsonDataExtractor = new JsonDataExtractor(json, EXCEPTION_ON_INVALID_PATH);

        Long[] timestampValues = jsonDataExtractor.parseLongArray(TIMESTAMP_PATH);

        Double[] closeValues = jsonDataExtractor.parseDoubleArray(CLOSE_PATH);
        Double[] adjCloseValues = jsonDataExtractor.parseDoubleArray(ADJ_QUOTE_PATH);

        // check if have minimal data
        if (ArrayUtils.isEmpty(timestampValues)) {
            // Two scenarios for this case:
            //   1. response has no data whatsoever (i.e. a date range w/ no data) === > return empty collection
            //   2. request was made with &includeTimestamps=false === > throw an exception
            if (ArrayUtils.isNotEmpty(closeValues) || ArrayUtils.isNotEmpty(adjCloseValues)) {
                throw new IllegalStateException("Cannot convert price history to map: Timestamps missing.");
            }
            return Collections.emptyList();
        }


        Double[] openValues = jsonDataExtractor.parseDoubleArray(OPEN_PATH);
        Double[] lowValues = jsonDataExtractor.parseDoubleArray(LOW_PATH);
        Double[] highValues = jsonDataExtractor.parseDoubleArray(HIGH_PATH);
        Long[] volumeValues = jsonDataExtractor.parseLongArray(VOLUME_PATH);


        List<Map<String, Number>> resultKeyValueList = new ArrayList<>();

        // ASSERT all lists are same length
        int entryCount = timestampValues.length;

        for (int i = 0; i < entryCount; i++)
        {
            Map<String,Number> entryMap = new HashMap<>();

            // will refactor iff slow performance is shown
            entryMap.put(KEY_TIMESTAMP, timestampValues[i]);
            if (ArrayUtils.isNotEmpty(openValues)) {
                entryMap.put(KEY_OPEN, openValues[i]);
            }
            if (ArrayUtils.isNotEmpty(closeValues)) {
                entryMap.put(KEY_CLOSE, closeValues[i]);
            }
            if (ArrayUtils.isNotEmpty(lowValues)) {
                entryMap.put(KEY_LOW, lowValues[i]);
            }
            if (ArrayUtils.isNotEmpty(highValues)) {
                entryMap.put(KEY_HIGH, highValues[i]);
            }
            if (ArrayUtils.isNotEmpty(volumeValues)) {
                entryMap.put(KEY_VOLUME, volumeValues[i]);
            }
            if (ArrayUtils.isNotEmpty(adjCloseValues)) {
                entryMap.put(KEY_ADJ_CLOSE, adjCloseValues[i]);
            }
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
