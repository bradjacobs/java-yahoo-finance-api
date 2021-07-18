package com.github.bradjacobs.yahoofinance.response;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsDateStrConverter;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SparkResponseConverter extends YahooResponseConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();
    //private static final JsonMapper mapper = JsonMapperSingleton.getPrettyInstance();


    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable

    private static final String KEY_SYMBOL = "symbol";
    private static final String KEY_PRICES = "prices";


    private static final EpochSecondsDateStrConverter epochSecondsToStringConverter = new EpochSecondsDateStrConverter();


    // todo - revisit the output structure for this
    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMaps(json);

        List<Map<String, Object>> resultListOfMaps = new ArrayList<>();

        // this is kinda dumb.. tacking on the symbol to the map otherwise doesn't know which values belong to which ticker.
        for (Map.Entry<String, Map<String, Object>> entry : mapOfMaps.entrySet())
        {
            String ticker = entry.getKey();
            Map<String, Object> timeCloseMap = entry.getValue();

            Map<String, Object> symbolEntryMap = new LinkedHashMap<>();
            symbolEntryMap.put(KEY_SYMBOL, ticker);
            symbolEntryMap.put(KEY_PRICES, timeCloseMap);
            resultListOfMaps.add(symbolEntryMap);
        }

        return resultListOfMaps;
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

            for (Map.Entry<String, Map<String, Object>> entry : originalMapOfMaps.entrySet())
            {
                String ticker = entry.getKey();
                Map<String, Object> tickerDateCloseMap = new TreeMap<>();

                Map<String, Object> dataMap = entry.getValue();
                List<Long> timestamps = (List<Long>) dataMap.get(KEY_TIMESTAMP);
                List<Number> closeValues = (List<Number>) dataMap.get(KEY_CLOSE);

                for (int i = 0; i < timestamps.size(); i++)
                {
                    Long timestamp = timestamps.get(i);
                    Number closeValue = closeValues.get(i);

                    Map<String,Object> entryMap = new LinkedHashMap<>();
                    String date = epochSecondsToStringConverter.convertToString(timestamp);
                    tickerDateCloseMap.put(date, closeValue);
                }

                resultMap.put(ticker, tickerDateCloseMap);
            }

            return resultMap;
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Unable to parse json: " + e.getMessage(), e);
        }
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
