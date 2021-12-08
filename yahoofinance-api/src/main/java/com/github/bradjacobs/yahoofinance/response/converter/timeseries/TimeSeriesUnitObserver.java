package com.github.bradjacobs.yahoofinance.response.converter.timeseries;

import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

abstract class TimeSeriesUnitObserver implements TimeSeriesResponseObserver
{
    private static final String UPPER_EBIT = "EBIT";
    private static final String LOWER_EBIT = "ebit";

    private static final String DATE_FIELD_NAME = "asOfDate";
    private static final String VALUE_FIELD_NAME = "reportedValue";

    private final TimeSeriesUnit type;
    private final String attributePrefix;
    private final int attributePrefixLength;
    private final Map<String, Map<String,Object>> valuesMap = new TreeMap<>();

    public TimeSeriesUnitObserver(TimeSeriesUnit type) {
        this.type = type;
        this.attributePrefix = type.toString().toLowerCase();
        this.attributePrefixLength = this.attributePrefix.length();
    }

    public TimeSeriesUnit getType() {
        return type;
    }

    @Override
    public void updateAttributeMap(String elementName, List<Map<String,Object>> elementDataList)
    {
        if (elementName == null || elementDataList == null || elementDataList.size() == 0) {
            return;
        }
        String attributeName = getBaseAttributeName(elementName);
        if (attributeName == null) {
            return;
        }

        for (Map<String, Object> entryMap : elementDataList) {
            if (entryMap != null) {
                String dateString = (String) entryMap.get(DATE_FIELD_NAME);
                Number value = (Number) entryMap.get(VALUE_FIELD_NAME);
                addMapEntry(attributeName, dateString, value);
            }
        }
    }

    abstract protected void addMapEntry(String attributeName, String dateString, Number value);


    /**
     * Converts full attribute name into the base name
     *   i.e.  annualNetIncomeContinuousOperations -->  netIncomeContinuousOperations
     * @param attributeName attributeName
     * @return return base attribute name.
     */
    //  ( dev note: did NOT see any perf improvement here with a caching solution )
    private String getBaseAttributeName(String attributeName)
    {
        if (! attributeName.startsWith(this.attributePrefix)) {
            return null;
        }
        String baseName = attributeName.substring(attributePrefixLength);

        // special case... ugh!!!
        if (baseName.equals(UPPER_EBIT)) {
            return LOWER_EBIT;
        }

        // baseName should start with lowercase letter
        return Character.toLowerCase(baseName.charAt(0)) + baseName.substring(1);
    }
}
