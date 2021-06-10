package com.github.bradjacobs.yahoofinance.response;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TimeSeriesResponseConverter extends YahooResponseConverter
{
    private static final String OUTPUT_FIELD_NAME_KEY = "fieldType";
    private static final String OUTPUT_FIELD_VALUES_KEY = "timeEntries";

    private static final String ROOT_PATH = "$.timeseries.result";
    private static final String ELEMENT_NAMES_PATH = ROOT_PATH + "[*].meta.type[0]";
    private static final String KEY_TIMESTAMP = "timestamp";


    private final boolean pruneEmptyEntries;

    public TimeSeriesResponseConverter(boolean pruneEmptyEntries)
    {
        this.pruneEmptyEntries = pruneEmptyEntries;
    }

    @Override
    protected String getPrimaryMapKeyName()
    {
        return OUTPUT_FIELD_NAME_KEY;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        json = JsonFormatRemover.removeFormats(json, true);

        DocumentContext jsonDoc = JsonPath.parse(json);

        ObjectMapper mapper =
            new ObjectMapper()
                .enable(SerializationFeature.INDENT_OUTPUT);

        // first fetch all the names (aka types) (aka names of the fields that were returned)
        String[] elementNames = jsonDoc.read(ELEMENT_NAMES_PATH, String[].class);

        int entryCount = elementNames.length;
        List<Map<String, Object>> resultTypeFieldList = new ArrayList<>();

        for (int i = 0; i < entryCount; i++)
        {
            String elementName = elementNames[i];

            List<Map<String,Object>> resultEntryList;
            try
            {
                Long[] timeValues = jsonDoc.read(constructEntryPath(i, KEY_TIMESTAMP), Long[].class);
                List<Map<String,Object>> entryData = jsonDoc.read(constructEntryPath(i, elementName));

                resultEntryList = new ArrayList<>();
                for (int j = 0; j < timeValues.length; j++)
                {
                    Long timestamp = timeValues[j];
                    Map<String,Object> dataMap = entryData.get(j);

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

            resultTypeFieldList.add(createFieldDataEntry(elementName, resultEntryList));
        }

        return resultTypeFieldList;
    }


    private Map<String, Object> createFieldDataEntry(String fieldName, List<Map<String,Object>> resultEntryList)
    {
        Map<String, Object> fieldDataMap = new LinkedHashMap<>();
        fieldDataMap.put(OUTPUT_FIELD_NAME_KEY, fieldName);
        fieldDataMap.put(OUTPUT_FIELD_VALUES_KEY, resultEntryList);
        return fieldDataMap;
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

}
