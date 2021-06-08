/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.json;

import com.github.bradjacobs.yahoofinance.model.PriceHistoryRecord;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
    private static final String BASE_PATH      = "$.chart.result[0]";
    private static final String TIMESTAMP_PATH = BASE_PATH + "." + KEY_TIMESTAMP;
    private static final String OPEN_PATH      = BASE_PATH + ".indicators.quote[0]." + KEY_OPEN;
    private static final String LOW_PATH       = BASE_PATH + ".indicators.quote[0]." + KEY_LOW;
    private static final String HIGH_PATH      = BASE_PATH + ".indicators.quote[0]." + KEY_HIGH;
    private static final String CLOSE_PATH     = BASE_PATH + ".indicators.quote[0]." + KEY_CLOSE;
    private static final String VOLUME_PATH    = BASE_PATH + ".indicators.quote[0]." + KEY_VOLUME;
    private static final String ADJ_CLOSE_PATH = BASE_PATH + ".indicators.adjclose[0]." + KEY_ADJ_CLOSE;


    private static final JsonMapper mapper = new JsonMapper();

    // temp hack for map value type until better soln
    public List<Map<String, Object>> toListOfMaps2(String json)
    {
        List<Map<String, Number>> resultList = toListOfMaps(json);

        List<Map<String,Object>> convertedList = new ArrayList<>();
        for (Map<String, Number> entryMap : resultList) {
            convertedList.add(entryMap.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
        }
        return convertedList;
    }


    public List<Map<String, Number>> toListOfMaps(String json)
    {
        // configure to return NULL (instead of Exception) if the _LEAF_ is missing
        //   b/c the field might not always be there.

        Configuration conf = Configuration.defaultConfiguration();
        conf = conf.addOptions(Option.DEFAULT_PATH_LEAF_TO_NULL);
        conf = conf.addOptions(Option.SUPPRESS_EXCEPTIONS);

        DocumentContext jsonDoc = JsonPath.using(conf).parse(json);

        Long[] timestampValues = jsonDoc.read(TIMESTAMP_PATH, Long[].class);

        Double[] closeValues = jsonDoc.read(CLOSE_PATH, Double[].class);
        Double[] adjCloseValues = jsonDoc.read(ADJ_CLOSE_PATH, Double[].class);
        boolean closeValuesExist = ArrayUtils.isNotEmpty(closeValues);
        boolean adjCloseValuesExist = ArrayUtils.isNotEmpty(adjCloseValues);

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

        // _ASSERT_ all lists are same length
        int entryCount = timestampValues.length;


        // assume that all of these exist for none
        Double[] openValues = jsonDoc.read(OPEN_PATH, Double[].class);
        Double[] lowValues = jsonDoc.read(LOW_PATH, Double[].class);
        Double[] highValues = jsonDoc.read(HIGH_PATH, Double[].class);
        Long[] volumeValues = jsonDoc.read(VOLUME_PATH, Long[].class);

        boolean openLowHighExists = ArrayUtils.isNotEmpty(openValues);

        List<Map<String, Number>> resultKeyValueList = new ArrayList<>();

        for (int i = 0; i < entryCount; i++)
        {
            Map<String,Number> entryMap = new HashMap<>();

            // will refactor iff slow performance is shown
            entryMap.put(KEY_TIMESTAMP, timestampValues[i]);
            if (openLowHighExists) {
                entryMap.put(KEY_OPEN, openValues[i]);
                entryMap.put(KEY_LOW, lowValues[i]);
                entryMap.put(KEY_HIGH, highValues[i]);
                entryMap.put(KEY_VOLUME, volumeValues[i]);
            }
            if (closeValuesExist) {
                entryMap.put(KEY_CLOSE, closeValues[i]);
            }
            if (adjCloseValuesExist) {
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
