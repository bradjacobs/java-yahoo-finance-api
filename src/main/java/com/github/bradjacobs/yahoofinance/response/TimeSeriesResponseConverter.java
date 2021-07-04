package com.github.bradjacobs.yahoofinance.response;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.response.helper.JsonFormatRemover;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Converts the original timeseries response JSON is to a more "nice" format  (subjectively)
 */
/*
   todo items:
      - better naming if possible
      - needs a lot of unit testing
      - need to decide on the 'expected' output format  (namely sorting on keys, etc)
      - might want to 'lop off' the day portion of the date so don't have "2021-06-25" "2021-06-26" as different map keys (though this "should" be rare)
      = code clean up (was originally done just to see it work)

 */
public class TimeSeriesResponseConverter implements YahooResponseConverter
{
    private static final String ROOT_PATH = "$.timeseries.result";
    private static final String ELEMENT_NAMES_PATH = ROOT_PATH + "[*].meta.type[0]";
    private static final String KEY_TIMESTAMP = "timestamp";

    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();

    private static final String ANNUAL_PREFIX = TimeSeriesUnit.ANNUAL.toString().toLowerCase();
    private static final String QUARTERLY_PREFIX = TimeSeriesUnit.QUARTERLY.toString().toLowerCase();
    private static final String TRAILING_PREFIX = TimeSeriesUnit.TRAILING.toString().toLowerCase();

    private static final String UPPER_EBIT = "EBIT";
    private static final String LOWER_EBIT = "ebit";

    private static final String TIMEFRAME_KEY = "timeType";  // todo - better name

    private final boolean pruneEmptyEntries;
    private final boolean orgainizeByDate;

    public TimeSeriesResponseConverter(boolean pruneEmptyEntries, boolean orgainizeByDate)
    {
        this.pruneEmptyEntries = pruneEmptyEntries;
        this.orgainizeByDate = orgainizeByDate;
    }


    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        if (orgainizeByDate) {
            return convertToListOfMapsDateKey(json);
        }
        else {
            return convertToListOfMapsAttributeKey(json);
        }
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        if (orgainizeByDate) {
            return createAlternateSignatureDateMap( convertToMapOfMapsDateKey(json) );
        }
        else {
            return convertToMapOfMapsAttributeKey(json);
        }
    }



    public List<Map<String, Object>> convertToListOfMapsAttributeKey(String json)
    {
        Map<String, Map<String, Object>> mapOfMaps = convertToMapOfMapsAttributeKey(json);

        List<Map<String, Object>> resultList = new ArrayList<>();
        List<String> mapKeys = new ArrayList<>(mapOfMaps.keySet());

        for (String mapKey : mapKeys)
        {
            Map<String, Object> mapData = mapOfMaps.get(mapKey);

            Map<String, Object> updatedMapData = new LinkedHashMap<>(); // linkedhashmap b/c want to force first entry.
            updatedMapData.put(TIMEFRAME_KEY, mapKey);
            updatedMapData.putAll(mapData);
            resultList.add(updatedMapData);
        }

        return resultList;
    }


    public List<Map<String, Object>> convertToListOfMapsDateKey(String json)
    {
        Map<String, Map<String, Map<String, Number>>> mapOfMaps = convertToMapOfMapsDateKey(json);

        List<Map<String, Object>> resultList = new ArrayList<>();
        List<String> mapKeys = new ArrayList<>(mapOfMaps.keySet());

        for (String mapKey : mapKeys)
        {
            Map<String, Map<String, Number>> mapData = mapOfMaps.get(mapKey);

            Map<String, Object> updatedMapData = new LinkedHashMap<>(); // linkedhashmap b/c want to force first entry.
            updatedMapData.put(TIMEFRAME_KEY, mapKey);
            updatedMapData.putAll(mapData);
            resultList.add(updatedMapData);
        }

        return resultList;
    }



    public Map<String, Map<String, Map<String, Number>>> convertToMapOfMapsDateKey(String json)
    {
        Map<String, Map<String, Object>> attributeMapOfMaps = convertToMapOfMapsAttributeKey(json);

        Map<String, Map<String, Map<String, Number>>> resultMap = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, Object>> entry : attributeMapOfMaps.entrySet())
        {
            String timeunitString = entry.getKey();
            Map<String, Object> timeunitDataMap = entry.getValue();

            Map<String, Map<String, Number>> timeSeriesDataMap = convertToDateMap(timeunitDataMap);
            resultMap.put(timeunitString, timeSeriesDataMap);
        }

        return resultMap;
    }


    private Map<String, Map<String, Number>> convertToDateMap(Map<String, Object> attributeDataMap)
    {
        if (attributeDataMap == null || attributeDataMap.size() == 0) {
            return Collections.emptyMap();
        }

        Map<String, Map<String, Number>> resultMap = new TreeMap<>();

        Map<String,List<TimeEntry>> mapOfTimeEntries = mapper.convertValue(attributeDataMap, new TypeReference<Map<String,List<TimeEntry>>>() {});

        for (Map.Entry<String, List<TimeEntry>> entry : mapOfTimeEntries.entrySet())
        {
            String attributeName = entry.getKey();
            List<TimeEntry> listOfTimeEntries = entry.getValue();

            for (TimeEntry timeEntry : listOfTimeEntries)
            {
                String date = timeEntry.getAsOfDate();
                Map<String, Number> dateAttributeMap = resultMap.computeIfAbsent(date, k -> new TreeMap<>());
                dateAttributeMap.put(attributeName, timeEntry.getReportedValue());
            }
        }

        return resultMap;
    }



    public Map<String, Map<String, Object>> convertToMapOfMapsAttributeKey(String json)
    {
        Map<String, Object> annualDataMap = new TreeMap<>();
        Map<String, Object> quarterlyDataMap = new TreeMap<>();
        Map<String, Object> trailingDataMap = new TreeMap<>();

        json = JsonFormatRemover.removeFormats(json, true);

        DocumentContext jsonDoc = JsonPath.parse(json);
        
        // first fetch all the names (aka types) (aka names of the fields that were returned)
        String[] elementNames = jsonDoc.read(ELEMENT_NAMES_PATH, String[].class);

        int entryCount = elementNames.length;

        for (int i = 0; i < entryCount; i++)
        {
            String elementName = elementNames[i];
            String prefix = null;

            Map<String, Object> destinationMap = null;
            if (elementName.startsWith(ANNUAL_PREFIX)) {
                destinationMap = annualDataMap;
                prefix = ANNUAL_PREFIX;
            }
            else if (elementName.startsWith(QUARTERLY_PREFIX)) {
                destinationMap = quarterlyDataMap;
                prefix = QUARTERLY_PREFIX;
            }
            else if (elementName.startsWith(TRAILING_PREFIX)) {
                destinationMap = trailingDataMap;
                prefix = TRAILING_PREFIX;
            }
            else {
                continue;
            }

            List<Map<String,Object>> resultEntryList;
            try
            {
                Long[] timeValues = jsonDoc.read(constructEntryPath(i, KEY_TIMESTAMP), Long[].class);
                List<Map<String,Object>> existingEntryDataList = jsonDoc.read(constructEntryPath(i, elementName));

                resultEntryList = new ArrayList<>();
                for (int j = 0; j < timeValues.length; j++)
                {
                    Long timestamp = timeValues[j];
                    Map<String,Object> dataMap = existingEntryDataList.get(j);

                    if (timestamp == null || dataMap == null) {
                        continue;
                    }
                    dataMap.put(KEY_TIMESTAMP, timestamp); // could be considered redundant b/c a string version of data is already in map
                    resultEntryList.add(dataMap);
                }
            }
            catch (Exception e) {
                // this exception will occur when trying to fetch timestamps on an entry with no data.
                resultEntryList = Collections.emptyList();
            }

            if (resultEntryList.size() == 0 && pruneEmptyEntries) {
                continue;
            }

            String cleanAttributeName = cleanAttributeName(elementName, prefix);

            destinationMap.put(cleanAttributeName, resultEntryList);
        }


        Map<String,Map<String, Object>> resultMap = new LinkedHashMap<>();
        resultMap.put(ANNUAL_PREFIX, annualDataMap);
        resultMap.put(QUARTERLY_PREFIX, quarterlyDataMap);
        resultMap.put(TRAILING_PREFIX, trailingDataMap);

        return resultMap;
    }


    private String constructEntryPath(int index, String pathSuffix)
    {
        // side: only avoiding using String.format b/c of perf concerns inside huge tight loops. (probably just paranoia)
        StringBuilder sb = new StringBuilder();
        sb.append(ROOT_PATH);
        sb.append('[');
        sb.append(index);
        sb.append("].");
        sb.append(pathSuffix);
        return sb.toString();
    }


    private String cleanAttributeName(String attributeName, String prefix)
    {
        String cleanName = attributeName.replace(prefix, "");

        // special case... ugh
        if (cleanName.equals(UPPER_EBIT)) {
            return LOWER_EBIT;
        }

        // todo: better perf ways to do this, but could require extra attention if get 'weird' string
        //    thus don't worry about performance (for now)
        char firstLetter = cleanName.charAt(0);
        if (Character.isUpperCase(firstLetter)) {
            cleanName = Character.toLowerCase(firstLetter) + cleanName.substring(1);
        }
        return cleanName;
    }

    /**
     * convert the next map value into a simple object
     * @param origDateResultMap
     * @return
     */
    //  side:  I know there's a simpler way
    private Map<String, Map<String, Object>> createAlternateSignatureDateMap(Map<String, Map<String, Map<String, Number>>> origDateResultMap)
    {
        Map<String, Map<String, Object>> resultMap = new LinkedHashMap<>();

        for (Map.Entry<String, Map<String, Map<String, Number>>> entry : origDateResultMap.entrySet())
        {
            String key = entry.getKey();
            Map<String, Map<String, Number>> map = entry.getValue();

            Map<String, Object> updatedMap = new LinkedHashMap<>(map);
            resultMap.put(key, updatedMap);
        }

        return resultMap;
    }


    // note: this class is really for convenience and may ultimately just get removed.
    private static class TimeEntry
    {
        private Long timestamp;
        private String dataId;
        private String asOfDate;
        private String periodType;
        private String currencyCode;
        private Number reportedValue;

        public Long getTimestamp() {
            return timestamp;
        }

        public String getDataId() {
            return dataId;
        }

        public String getAsOfDate() {
            return asOfDate;
        }

        public String getPeriodType() {
            return periodType;
        }

        public String getCurrencyCode() {
            return currencyCode;
        }

        public Number getReportedValue() {
            return reportedValue;
        }
    }
}
