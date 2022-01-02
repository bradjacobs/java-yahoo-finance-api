package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.response.converter.util.SimpleMapOfMapsGenerator;
import com.github.bradjacobs.yahoofinance.util.JsonConverter;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ChartResponseConverter implements ResponseConverter
{
    private static final String KEY_CHART = "chart";
    private static final String KEY_RESULT = "result";
    private static final String KEY_INDICATORS = "indicators";
    private static final String KEY_QUOTE = "quote";

    // key titles for the output map
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_OPEN = "open";
    private static final String KEY_LOW = "low";
    private static final String KEY_HIGH = "high";
    private static final String KEY_CLOSE = "close";
    private static final String KEY_VOLUME = "volume";
    private static final String KEY_ADJ_CLOSE = "adjclose";

    private static final String KEY_DATE = "date";  // extra that converts timestamp to human-readable

    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();
    private static final EpochStrConverter dateStrConverter = MetaEpochSecondsConverter.getDateStringConverter();

    private static final TypeReference<Map<String, List<Object>>> REF_MAP_OF_LISTS = new TypeReference<Map<String, List<Object>>>(){};


    private final SimpleMapOfMapsGenerator mapOfMapsGenerator = new SimpleMapOfMapsGenerator(KEY_DATE, false); // todo - fix bool param

    public ChartResponseConverter()
    {
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return mapOfMapsGenerator.convertToMap( this.convertToListOfMaps(json) );
    }


    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        JsonNode rootNode = JsonConverter.convertToNode(json);
        JsonNode chartNode = rootNode.get(KEY_CHART);
        JsonNode resultNode = chartNode.get(KEY_RESULT);
        JsonNode resultNodeElementZero = resultNode.get(0);

        JsonNode timestampNode = resultNodeElementZero.get(KEY_TIMESTAMP);

        JsonNode indicatorsNode = resultNodeElementZero.get(KEY_INDICATORS);
        JsonNode quoteNode = indicatorsNode.get(KEY_QUOTE);
        JsonNode quoteNodeElementZero = quoteNode.get(0);
        Map<String, List<Object>> quoteValueMap = mapper.convertValue(quoteNodeElementZero, REF_MAP_OF_LISTS);

        List<Object> openValueList = quoteValueMap.get(KEY_OPEN);
        List<Object> highValueList = quoteValueMap.get(KEY_HIGH);
        List<Object> lowValueList = quoteValueMap.get(KEY_LOW);
        List<Object> closeValueList = quoteValueMap.get(KEY_CLOSE);
        List<Object> volumeValueList = quoteValueMap.get(KEY_VOLUME);

        List<Object> adjCloseValueList = null;
        JsonNode adjQuoteNode = indicatorsNode.get(KEY_ADJ_CLOSE);
        if (adjQuoteNode != null) {
            JsonNode adjQuoteNodeElementZero = adjQuoteNode.get(0);
            Map<String, List<Object>> adjQuoteValueMap = mapper.convertValue(adjQuoteNodeElementZero, REF_MAP_OF_LISTS);
            adjCloseValueList = adjQuoteValueMap.get(KEY_ADJ_CLOSE);
        }

        Long[] timestampValues = null;
        if (timestampNode != null) {
            timestampValues = mapper.convertValue(timestampNode, Long[].class);
        }

        // Two scenarios for this case:
        //   1. response has no data whatsoever (i.e. a date range w/ no data) === > return empty collection
        //   2. request was made with &includeTimestamps=false === > throw an exception
        if (timestampValues == null || timestampValues.length == 0) {
            if ((closeValueList != null && closeValueList.size() > 0) ||
                    (adjCloseValueList != null && adjCloseValueList.size() > 0)) {
                throw new IllegalStateException("Cannot convert price history to map: Timestamps missing.");
            }
            return Collections.emptyList();
        }

        List<Map<String, Object>> resultKeyValueList = new ArrayList<>();


        for (int i = 0; i < timestampValues.length; i++)
        {
            // note: enforcing output fields to be in a certain order
            Map<String,Object> entryMap = new LinkedHashMap<>();

            Long timestamp = timestampValues[i];
            entryMap.put(KEY_DATE, dateStrConverter.convertToString(timestamp));

            if (openValueList != null) {
                // if open exists, then high and low will always exist
                entryMap.put(KEY_OPEN, openValueList.get(i));
                entryMap.put(KEY_HIGH, highValueList.get(i));
                entryMap.put(KEY_LOW, lowValueList.get(i));
            }

            if (closeValueList != null) {
                entryMap.put(KEY_CLOSE, closeValueList.get(i));
            }
            if (adjCloseValueList != null) {
                entryMap.put(KEY_ADJ_CLOSE, adjCloseValueList.get(i));
            }
            if (volumeValueList != null) {
                entryMap.put(KEY_VOLUME, volumeValueList.get(i));
            }
            resultKeyValueList.add(entryMap);
        }

        return resultKeyValueList;
    }

}
