package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SparkResponseConverter implements ResponseConverter
{
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable

    // TODO - fix dossn't allow for timestamps more granular than day.
    private static final EpochSecondsConverter EPOCH_SECONDS_CONVERTER = new EpochSecondsConverter();

    private static final DefaultResponseConverter defaultConverter = new DefaultResponseConverter();

    // TODO - FIX - bug to fix.  organizeByDate = true is broken if ticker count > batchSize
    //   (the dates in the map will get overwritten)
    private static final boolean DEFAULT_ORGANIZE_BY_DATE = false;
    private final boolean organizeByDate;

    public SparkResponseConverter() {
        this(DEFAULT_ORGANIZE_BY_DATE);
    }
    public SparkResponseConverter(boolean organizeByDate) {
        this.organizeByDate = organizeByDate;
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
                String date = EPOCH_SECONDS_CONVERTER.toString(timestamp);

                if (organizeByDate)
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
