package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;
import com.github.bradjacobs.yahoofinance.response.converter.util.JsonNestedFormatRemover;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

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
    private static final String ROOT_PATH = "$.timeseries.result";
    private static final String ELEMENT_NAMES_PATH = ROOT_PATH + "[*].meta.type[0]";
    private static final String KEY_TIMESTAMP = "timestamp";

    private static final String ANNUAL_PREFIX = TimeSeriesUnit.ANNUAL.toString().toLowerCase();
    private static final String QUARTERLY_PREFIX = TimeSeriesUnit.QUARTERLY.toString().toLowerCase();
    private static final String TRAILING_PREFIX = TimeSeriesUnit.TRAILING.toString().toLowerCase();

    private static final String UPPER_EBIT = "EBIT";
    private static final String LOWER_EBIT = "ebit";

    private final boolean organizeByDate;
    private final JsonNestedFormatRemover jsonNestedFormatRemover = new JsonNestedFormatRemover(true);

    public TimeSeriesResponseConverter() {
        this(null);
    }

    public TimeSeriesResponseConverter(ResponseConverterConfig config) {
        this.organizeByDate = (config == null || config.isUseDateAsMapKey());
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
        if (attributeMapPojo.hasMultipleTimeFrames())
        {
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
        else
        {
            return attributeMapPojo.getSinglePopulatedMap();
        }
    }



    public Map<String, Object> createAltMapSignature(Map<String, Map<String, Object>> origDateResultMap)
    {
        return new LinkedHashMap<>(origDateResultMap);
    }



    private AttributeMapPojo extractAttributeDataValueInfo(String json)
    {
        Map<String, Map<String,Object>> annualValuesMap = new TreeMap<>();
        Map<String, Map<String,Object>> quarterlyValuesMap = new TreeMap<>();
        Map<String, Map<String,Object>> trailingValuesMap = new TreeMap<>();

        json = jsonNestedFormatRemover.removeFormats(json);

        DocumentContext jsonDoc = JsonPath.parse(json);

        // first fetch all the names (aka types) (aka names of the fields that were returned)
        String[] elementNames = jsonDoc.read(ELEMENT_NAMES_PATH, String[].class);

        int entryCount = elementNames.length;
        for (int i = 0; i < entryCount; i++)
        {
            String elementName = elementNames[i];
            String prefix;

            Map<String, Map<String,Object>> destinationMap = null;
            if (elementName.startsWith(ANNUAL_PREFIX)) {
                destinationMap = annualValuesMap;
                prefix = ANNUAL_PREFIX;
            }
            else if (elementName.startsWith(QUARTERLY_PREFIX)) {
                destinationMap = quarterlyValuesMap;
                prefix = QUARTERLY_PREFIX;
            }
            else if (elementName.startsWith(TRAILING_PREFIX)) {
                destinationMap = trailingValuesMap;
                prefix = TRAILING_PREFIX;
            }
            else {
                continue;
            }

            String attributeName = cleanAttributeName(elementName, prefix);

            try
            {
                Long[] timeValues = jsonDoc.read(constructEntryPath(i, KEY_TIMESTAMP), Long[].class);
                List<Map<String,Object>> existingEntryDataList = jsonDoc.read(constructEntryPath(i, elementName));

                for (int j = 0; j < timeValues.length; j++)
                {
                    Long timestamp = timeValues[j];
                    Map<String,Object> dataMap = existingEntryDataList.get(j);

                    // must be a value in BOTH the timeValues array and the dataList
                    if (timestamp == null || dataMap == null) {
                        continue;
                    }

                    String dateString = (String) dataMap.get("asOfDate");
                    Number value = (Number) dataMap.get("reportedValue");

                    if (this.organizeByDate)
                    {
                        // get map containing all the attributes for this given date.
                        Map<String, Object> attributeMap = destinationMap.computeIfAbsent(dateString, k -> new TreeMap<>());
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
            catch (Exception e) {
                // this exception will occur when trying to fetch timestamps on an entry with no data.
            }
        }

        return new AttributeMapPojo(annualValuesMap, quarterlyValuesMap, trailingValuesMap);
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
            if (this.annualDataMap.size() > 1) {
                return this.annualDataMap;
            }
            else if (this.quarterlyDataMap.size() > 1) {
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
