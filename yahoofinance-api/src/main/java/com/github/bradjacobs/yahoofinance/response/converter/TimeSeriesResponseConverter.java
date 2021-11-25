package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.util.JsonNestedFormatRemover;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;
import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Converts the original timeseries response JSON is to a more "nice" format  (subjectively)
 */
/*
   todo items:
      - needs a lot of unit testing
      - might want to 'lop off' the day portion of the date so don't have "2021-06-25" "2021-06-26" as different map keys (though this "should" be rare)
      = code clean up (was originally done just to see it work)

 */
public class TimeSeriesResponseConverter implements ResponseConverter
{
    private static final String RESULT_OBJECTS_PATH = "$.timeseries.result[*]";
    private static final String ELEMENT_NAMES_PATH = RESULT_OBJECTS_PATH + ".meta.type[0]";

    private static final String TIMESTAMP_FIELD_NAME = "timestamp";
    private static final String DATE_FIELD_NAME = "asOfDate";
    private static final String VALUE_FIELD_NAME = "reportedValue";

    private static final String EXTRA_DATE_STRING_LABEL = "date";

    private static final String ANNUAL_PREFIX = TimeSeriesUnit.ANNUAL.toString().toLowerCase();
    private static final String QUARTERLY_PREFIX = TimeSeriesUnit.QUARTERLY.toString().toLowerCase();
    private static final String TRAILING_PREFIX = TimeSeriesUnit.TRAILING.toString().toLowerCase();

    private static final String UPPER_EBIT = "EBIT";
    private static final String LOWER_EBIT = "ebit";
    private static final boolean DEFAULT_ORGANIZE_BY_DATE = true;

    private final boolean organizeByDate;
    private final JsonNestedFormatRemover jsonNestedFormatRemover = new JsonNestedFormatRemover(true);
    private final JsonPathDocContextCreator jsonPathDocContextCreator = new JsonPathDocContextCreator();


    public TimeSeriesResponseConverter() {
        this(DEFAULT_ORGANIZE_BY_DATE);
    }

    public TimeSeriesResponseConverter(boolean organizeByDate) {
        this.organizeByDate = organizeByDate;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        throw new UnsupportedOperationException("Returning data in List format is not supported for timeseries data.");
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        AttributeMapPojo attributeMapPojo = extractAttributeDataValueInfo(json);

        // todo - this return format is TBD.   namely the resulting map has a different structure
        //   if only request a single timeUnit vs multiple.
        if (attributeMapPojo.hasMultipleTimeFrames()) {
            // this case we have at least 2 of the following:
            //     annual, quarterly, trailing
            //
            // thus to 'try' to keep them separate, create an uber map
            Map<String,Map<String, Object>> metaMap = new LinkedHashMap<>();
            metaMap.put(ANNUAL_PREFIX, createAltMapSignature( attributeMapPojo.getAnnualDataMap() ));
            metaMap.put(QUARTERLY_PREFIX, createAltMapSignature( attributeMapPojo.getQuarterlyDataMap() ));
            metaMap.put(TRAILING_PREFIX, createAltMapSignature( attributeMapPojo.getTrailingDataMap() ));
            return metaMap;
        }
        else {
            return attributeMapPojo.getSinglePopulatedMap();
        }
    }

    public Map<String, Object> createAltMapSignature(Map<String, Map<String, Object>> origDateResultMap)
    {
        return new LinkedHashMap<>(origDateResultMap);
    }

    private TimeSeriesUnit getTimeSeriesType(String elementName) {
        if (elementName.startsWith(ANNUAL_PREFIX)) {
            return TimeSeriesUnit.ANNUAL;
        }
        else if (elementName.startsWith(QUARTERLY_PREFIX)) {
            return TimeSeriesUnit.QUARTERLY;
        }
        else if (elementName.startsWith(TRAILING_PREFIX)) {
            return TimeSeriesUnit.TRAILING;
        }
        else {
            return null;
        }
    }

    private AttributeMapPojo extractAttributeDataValueInfo(String json)
    {
        Map<String, Map<String,Object>> annualValuesMap = new TreeMap<>();
        Map<String, Map<String,Object>> quarterlyValuesMap = new TreeMap<>();
        Map<String, Map<String,Object>> trailingValuesMap = new TreeMap<>();

        Map<TimeSeriesUnit, Map<String, Map<String,Object>>> timeSeriesDataMap = new HashMap<>();
        timeSeriesDataMap.put(TimeSeriesUnit.ANNUAL, annualValuesMap);
        timeSeriesDataMap.put(TimeSeriesUnit.QUARTERLY, quarterlyValuesMap);
        timeSeriesDataMap.put(TimeSeriesUnit.TRAILING, trailingValuesMap);

        json = jsonNestedFormatRemover.removeFormats(json);

        DocumentContext jsonDoc = jsonPathDocContextCreator.createDocContext(json);

        // first fetch all the names (aka types) (aka names of the fields that were returned)
        String[] elementNames = jsonDoc.read(ELEMENT_NAMES_PATH, String[].class);

        List<Map<String,Object>> resultsDataList = jsonDoc.read(RESULT_OBJECTS_PATH);

        int entryCount = elementNames.length;
        for (int i = 0; i < entryCount; i++)
        {
            String elementName = elementNames[i];
            TimeSeriesUnit type = getTimeSeriesType(elementName);
            if (type == null) {
                continue;
            }

            Map<String, Map<String,Object>> destinationMap = timeSeriesDataMap.get(type);

            Map<String, Object> attributeDataMap = resultsDataList.get(i);
            List<Long> timeValues = (List<Long>) attributeDataMap.get(TIMESTAMP_FIELD_NAME);
            if (timeValues == null) {
                continue;
            }

            List<Map<String,Object>> elementDataList = (List<Map<String, Object>>) attributeDataMap.get(elementName);
            if (elementDataList == null) {
                continue;
            }

            String attributeName = getBaseAttributeName(elementName, type);

            int timeValueCount = timeValues.size();
            for (int j = 0; j < timeValueCount; j++)
            {
                Map<String, Object> elementDataEntry = elementDataList.get(j);
                if (elementDataEntry == null) {
                    // possible that don't have values for some timestamps (usually for 'older' timestamps)
                    continue;
                }
                String dateString = (String) elementDataEntry.get(DATE_FIELD_NAME);
                Number value = (Number) elementDataEntry.get(VALUE_FIELD_NAME);

                if (this.organizeByDate)
                {
                    // get map containing all the attributes for this given date.
                    Map<String, Object> attributeMap = destinationMap.get(dateString);
                    if (attributeMap == null) {
                        attributeMap = new TreeMap<>();
                        attributeMap.put(EXTRA_DATE_STRING_LABEL, dateString);  // very convenient to have date in this location as well.
                        destinationMap.put(dateString, attributeMap);
                    }

                    attributeMap.put(attributeName, value);
                }
                else
                {
                    // get map containing all the dates for this given attribute.
                    Map<String, Object> dateMap = destinationMap.computeIfAbsent(attributeName, k -> new TreeMap<>());
                    dateMap.put(dateString, value);
                }
            }
        }

        return new AttributeMapPojo(annualValuesMap, quarterlyValuesMap, trailingValuesMap);
    }


    /**
     * Converts full attribute name into the base name
     *   i.e.  annualNetIncomeContinuousOperations -->  netIncomeContinuousOperations
     * @param attributeName attributeName
     * @param type identifies type annual/quarterly/trailing
     * @return return base attribuet name.
     */
    //  ( dev note: did NOT see any perf improvement here with a caching solution )
    private String getBaseAttributeName(String attributeName, TimeSeriesUnit type)
    {
        // first remove the prefix, which happens to be the same length as the enum.
        String baseName = attributeName.substring(type.toString().length());

        // special case... ugh
        if (baseName.equals(UPPER_EBIT)) {
            return LOWER_EBIT;
        }

        char firstLetter = baseName.charAt(0);
        if (Character.isUpperCase(firstLetter)) {
            baseName = Character.toLowerCase(firstLetter) + baseName.substring(1);
        }
        return baseName;
    }



    /**
     *  Each map is either in the form
     *    Attribute
     *         Date1 -> Value1
     *         Date2 -> Value2
     *         Date3 -> Value3
     *
     *       OR
     *
     *    Date
     *         Attribute1 -> Value1
     *         Attribute2 -> Value2
     *         Attribute3 -> Value3
     */
    private static class AttributeMapPojo
    {
        private final Map<String, Map<String,Object>> annualDataMap;
        private final Map<String, Map<String,Object>> quarterlyDataMap;
        private final Map<String, Map<String,Object>> trailingDataMap;
        private final boolean hasMultipleTimeFrames;

        public AttributeMapPojo(
                Map<String, Map<String,Object>> annualDataMap,
                Map<String, Map<String,Object>> quarterlyDataMap,
                Map<String, Map<String,Object>> trailingDataMap)
        {
            this.annualDataMap = annualDataMap;
            this.quarterlyDataMap = quarterlyDataMap;
            this.trailingDataMap = trailingDataMap;

            int timeFrameCount = 0;
            if (this.annualDataMap.size() > 0) { timeFrameCount++; }
            if (this.quarterlyDataMap.size() > 0) { timeFrameCount++; }
            if (this.trailingDataMap.size() > 0) { timeFrameCount++; }

            this.hasMultipleTimeFrames = (timeFrameCount > 1);
        }

        public Map<String, Map<String, Object>> getAnnualDataMap() {
            return annualDataMap;
        }

        public Map<String, Map<String, Object>> getQuarterlyDataMap() {
            return quarterlyDataMap;
        }

        public Map<String, Map<String, Object>> getTrailingDataMap() {
            return trailingDataMap;
        }

        public Map<String, Map<String, Object>> getSinglePopulatedMap() {
            if (this.hasMultipleTimeFrames) {
                return null;
            }

            // todo -- revisit....weird logic
            if (this.annualDataMap.size() > 0) {
                return this.annualDataMap;
            }
            else if (this.quarterlyDataMap.size() > 0) {
                return this.quarterlyDataMap;
            }
            else {
                return this.trailingDataMap;
            }
        }

        public boolean hasMultipleTimeFrames() {
            return hasMultipleTimeFrames;
        }
    }
}