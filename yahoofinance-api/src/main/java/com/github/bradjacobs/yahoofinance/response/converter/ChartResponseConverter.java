package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;
import com.github.bradjacobs.yahoofinance.response.helper.ListToMapConverter;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ChartResponseConverter extends YahooResponseConverter
{
    // key titles for the output map
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_OPEN = "open";
    private static final String KEY_LOW = "low";
    private static final String KEY_HIGH = "high";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_VOLUME = "volume";
    private static final String KEY_ADJ_CLOSE = "adjclose";

    //   todo: for now date does NOT have any hh/mm/ss, but that would probably be desired if querying on in-day values.
    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable


    // path locations for data within the JSON response
    private static final String BASE_PATH      = "$.chart.result[0]";
    private static final String TIMESTAMP_PATH = BASE_PATH + "." + KEY_TIMESTAMP;
    private static final String OPEN_PATH      = BASE_PATH + ".indicators.quote[0]." + KEY_OPEN;
    private static final String LOW_PATH       = BASE_PATH + ".indicators.quote[0]." + KEY_LOW;
    private static final String HIGH_PATH      = BASE_PATH + ".indicators.quote[0]." + KEY_HIGH;
    private static final String CLOSE_PATH     = BASE_PATH + ".indicators.quote[0]." + KEY_CLOSE;
    private static final String VOLUME_PATH    = BASE_PATH + ".indicators.quote[0]." + KEY_VOLUME;
    private static final String ADJ_CLOSE_PATH = BASE_PATH + ".indicators.adjclose[0]." + KEY_ADJ_CLOSE;


    // if 2 adjacent timestamps are within this interval threshold, then consider it 'small interval'
    //   and use 'datetime' instead of 'date' for string representation.
    private static final long SMALL_TIMESTAMP_INTERVAL_SECONDS = 60 * 60 * 23; // (23 hours in seconds)


    private static final ResponseConverterConfig defaultResponseConverterConfig = ResponseConverterConfig.DEFAULT_INSTANCE;



    // configure to return NULL (instead of Exception) if the _LEAF_ is missing
    //   b/c the field might not always be there.
    private static final Configuration JSON_PATH_CONFIG =
        Configuration.defaultConfiguration()
            .addOptions(Option.DEFAULT_PATH_LEAF_TO_NULL)
            .addOptions(Option.SUPPRESS_EXCEPTIONS);


    private final ResponseConverterConfig config;


    public ChartResponseConverter() {
        this(null);
    }

    public ChartResponseConverter(ResponseConverterConfig config) {
        if (config == null) {
            config = defaultResponseConverterConfig; // if null, use instance w/ default values.
        }
        this.config = config;
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return ListToMapConverter.convertToMap(KEY_DATE, convertToListOfMaps(json));
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        DocumentContext jsonDoc = JsonPath.using(JSON_PATH_CONFIG).parse(json);

        Long[] timestampValues = jsonDoc.read(TIMESTAMP_PATH, Long[].class);

        Number[] closeValues = jsonDoc.read(CLOSE_PATH, Number[].class);
        Number[] adjCloseValues = jsonDoc.read(ADJ_CLOSE_PATH, Number[].class);
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

        EpochStrConverter epochStrConverter = selectDateConverter(timestampValues);

        // _ASSERT_ all lists are same length
        int entryCount = timestampValues.length;

        // assume that all (open/high/low/volume) exist or none of them do.
        Number[] openValues = jsonDoc.read(OPEN_PATH, Number[].class);

        boolean openLowHighExists = ArrayUtils.isNotEmpty(openValues);

        // if there are no 'open' values, then assume there are no low/high/volume values either
        //   thus don't waste the time to attempt to parse & load.
        Number[] lowValues = null;
        Number[] highValues = null;
        Long[] volumeValues = null;

        if (openLowHighExists) {
            lowValues = jsonDoc.read(LOW_PATH, Number[].class);
            highValues = jsonDoc.read(HIGH_PATH, Number[].class);
            volumeValues = jsonDoc.read(VOLUME_PATH, Long[].class);
        }


        List<Map<String, Object>> resultKeyValueList = new ArrayList<>();

        for (int i = 0; i < entryCount; i++)
        {
            Map<String,Object> entryMap = new HashMap<>();

            // will refactor iff slow performance is shown
            Long timestamp = timestampValues[i];
            entryMap.put(KEY_DATE, epochStrConverter.convertToString(timestamp));
            entryMap.put(KEY_TIMESTAMP, timestamp);

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


    private EpochStrConverter selectDateConverter(Long[] timestampValues)
    {
        if (this.config.isAutoDetechDateTime())
        {
            if (timestampValues != null && timestampValues.length > 1)
            {
                Long timestamp1 = timestampValues[0];
                Long timestamp2 = timestampValues[1];
                if (timestamp1 != null && timestamp2 != null)
                {
                    if (Math.abs(timestamp1 - timestamp2) < SMALL_TIMESTAMP_INTERVAL_SECONDS) {
                        return MetaEpochSecondsConverter.getDateTimeStringConverter();
                    }
                }
            }
        }

        return MetaEpochSecondsConverter.getDateStringConverter();
    }

}
