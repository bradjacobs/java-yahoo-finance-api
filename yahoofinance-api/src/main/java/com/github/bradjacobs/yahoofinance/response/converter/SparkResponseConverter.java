package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SparkResponseConverter implements ResponseConverter
{
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable

    private static final DefaultResponseConverter defaultConverter = new DefaultResponseConverter();

    private final ResponseConverterConfig config;

    public SparkResponseConverter() {
        this(null);
    }

    public SparkResponseConverter(ResponseConverterConfig config) {
        if (config == null) {
            config = ResponseConverterConfig.DEFAULT_INSTANCE; // if null, use instance w/ default values.
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

        Map<String, Map<String, Object>> originalMapOfMaps = defaultConverter.convertToMapOfMaps(json);

        EpochStrConverter epochStrConverter = null;
        boolean orgainizeByDate = this.config.isUseDateAsMapKey();

        for (Map.Entry<String, Map<String, Object>> entry : originalMapOfMaps.entrySet())
        {
            String ticker = entry.getKey();

            Map<String, Object> dataMap = entry.getValue();
            List<Long> timestamps = (List<Long>) dataMap.get(KEY_TIMESTAMP);
            List<Number> closeValues = (List<Number>) dataMap.get(KEY_CLOSE);

            if (epochStrConverter == null) {
                // todo - fix... moved method to a common location, but it is still ugly
                epochStrConverter = MetaEpochSecondsConverter.selectDateStrConverter(timestamps.toArray(new Long[0]), this.config.isAutoDetechDateTime());
            }

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
