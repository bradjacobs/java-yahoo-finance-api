package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsDateStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsDateTimeStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SparkResponseConverter extends YahooResponseConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();


    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable


    // if 2 adjacent timestamps are within this interval threshold, then consider it 'small interval'
    //   and use 'datetime' instead of 'date' for string representation.
    private static final long SMALL_TIMESTAMP_INTERVAL_SECONDS = 60 * 60 * 23; // (23 hours in seconds)


    private static final EpochStrConverter epochSecondsToDateStringConverter = new EpochSecondsDateStrConverter();
    private static final EpochStrConverter epochSecondsToDateTimeStringConverter = new EpochSecondsDateTimeStrConverter();
    private static final ResponseConverterConfig defaultResponseConverterConfig = ResponseConverterConfig.DEFAULT_INSTANCE;


    private final ResponseConverterConfig config;

    public SparkResponseConverter() {
        this(null);
    }

    public SparkResponseConverter(ResponseConverterConfig config) {
        if (config == null) {
            config = defaultResponseConverterConfig; // if null, use instance w/ default values.
        }
        this.config = config;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        throw new UnsupportedOperationException("Returning data in List format is not supported for spark data.");
    }

    /**
     * Converts into a Map with form
     *   {
     *   "AAPL" : {
     *     "2021-07-02" : 139.96,
     *     "2021-07-06" : 142.02,
     *     "2021-07-07" : 144.57,
     *     ...
     *   },
     *   "MSFT" : {
     *     "2021-07-02" : 277.65,
     *     ...
     *   }
     * }
     * @param json
     * @return
     */
    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        Map<String, Map<String, Object>> resultMap = new TreeMap<>();

        try {
            Map<String, Map<String, Object>> originalMapOfMaps =
                mapper.readValue(json, new TypeReference<Map<String, Map<String, Object>>>() {});

            EpochStrConverter epochStrConverter = selectDateConverter(originalMapOfMaps);

            boolean orgainizeByDate = this.config.isUseDateAsMapKey();

            for (Map.Entry<String, Map<String, Object>> entry : originalMapOfMaps.entrySet())
            {
                String ticker = entry.getKey();

                Map<String, Object> dataMap = entry.getValue();
                List<Long> timestamps = (List<Long>) dataMap.get(KEY_TIMESTAMP);
                List<Number> closeValues = (List<Number>) dataMap.get(KEY_CLOSE);

                for (int i = 0; i < timestamps.size(); i++)
                {
                    Long timestamp = timestamps.get(i);
                    Number closeValue = closeValues.get(i);
                    String date = epochStrConverter.convertToString(timestamp);

                    if (orgainizeByDate)
                    {
                        Map<String, Object> internalTickerCloseMap = resultMap.computeIfAbsent(date, k -> new TreeMap<>());
                        internalTickerCloseMap.put(ticker, closeValue);
                    }
                    else {
                        Map<String, Object> internalDateCloseMap = resultMap.computeIfAbsent(ticker, k -> new TreeMap<>());
                        internalDateCloseMap.put(date, closeValue);
                    }
                }
            }

            return resultMap;
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to parse json: " + e.getMessage(), e);
        }
    }

    private EpochStrConverter selectDateConverter(Map<String, Map<String, Object>> mapOfMaps)
    {
        if (this.config.isAutoDetechDateTime()) {
            String firstTickerKey = new ArrayList<>(mapOfMaps.keySet()).get(0);
            Map<String, Object> tickerDataMap = mapOfMaps.get(firstTickerKey);

            if (tickerDataMap != null && tickerDataMap.size() > 0)
            {
                List<Long> timestamps = (List<Long>) tickerDataMap.get(KEY_TIMESTAMP);
                if (timestamps != null && timestamps.size() > 1)
                {
                    Long timestamp1 = timestamps.get(0);
                    Long timestamp2 = timestamps.get(1);
                    if (timestamp1 != null && timestamp2 != null)
                    {
                        if (Math.abs(timestamp1 - timestamp2) < SMALL_TIMESTAMP_INTERVAL_SECONDS) {
                            return epochSecondsToDateTimeStringConverter;
                        }
                    }
                }
            }
        }

        return epochSecondsToDateStringConverter;
    }


    //  {
    //  "AAPL" : {
    //    "timestamp" : [
    //      1625232600,
    //      1625578200,
    //      1625664600,
    //      1625751000,
    //      1625860802
    //    ],
    //    "symbol" : "AAPL",
    //    "dataGranularity" : 300,
    //    "close" : [
    //      139.96,
    //      142.02,
    //      144.57,
    //      143.24,
    //      145.11
    //    ],
    //    "end" : null,
    //    "start" : null,
    //    "previousClose" : null,
    //    "chartPreviousClose" : 137.27
    //  }
    //}
}
