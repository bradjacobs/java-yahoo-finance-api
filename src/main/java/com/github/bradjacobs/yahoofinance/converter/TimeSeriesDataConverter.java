package com.github.bradjacobs.yahoofinance.converter;

import com.github.bradjacobs.yahoofinance.model.TimeEntry;
import com.github.bradjacobs.yahoofinance.model.TimeSeriesResult;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

public class TimeSeriesDataConverter
{
    private static final String ANNUAL_PREFIX = TimeSeriesUnit.ANNUAL.toString().toLowerCase();
    private static final String QUARTERLY_PREFIX = TimeSeriesUnit.QUARTERLY.toString().toLowerCase();
    private static final String TRAILING_PREFIX = TimeSeriesUnit.TRAILING.toString().toLowerCase();

    private static final String UPPER_EBIT = "EBIT";
    private static final String LOWER_EBIT = "ebit";


    public Map<String, Map<String, Map<String, Number>>> convertToDateMap(List<TimeSeriesResult> timeResults)
    {
        Map<String, Map<String, Number>> annualDateMap = extarctAnnualDateMap(timeResults);
        Map<String, Map<String, Number>> quarterlyDateMap = extarctQuarterlyDateMap(timeResults);
        Map<String, Map<String, Number>> trailingDateMap = extarctTrailingDateMap(timeResults);

        Map<String, Map<String, Map<String, Number>>> resultMap = new LinkedHashMap<>();
        resultMap.put(ANNUAL_PREFIX, annualDateMap);
        resultMap.put(QUARTERLY_PREFIX, quarterlyDateMap);
        resultMap.put(TRAILING_PREFIX, trailingDateMap);

        return resultMap;
    }


    public Map<String, Map<String, Number>> extarctAnnualDateMap(List<TimeSeriesResult> timeResults)
    {
        return createDateMap(timeResults, ANNUAL_PREFIX);
    }
    public Map<String, Map<String, Number>> extarctQuarterlyDateMap(List<TimeSeriesResult> timeResults)
    {
        return createDateMap(timeResults, QUARTERLY_PREFIX);
    }
    public Map<String, Map<String, Number>> extarctTrailingDateMap(List<TimeSeriesResult> timeResults)
    {
        return createDateMap(timeResults, TRAILING_PREFIX);
    }



    private Map<String, Map<String, Number>> createDateMap(List<TimeSeriesResult> timeResults, String prefix)
    {
        Map<String,Map<String,Number>> resultMap = new TreeMap<>();

        timeResults = filterResults(timeResults, prefix);

        for (TimeSeriesResult timeResult : timeResults)
        {
            String attributeName = timeResult.getFieldType();
            List<TimeEntry> entries = timeResult.getTimeEntries();

         //   attributeName = cleanAttributeName(attributeName, prefix);

            for (TimeEntry timeEntry : entries)
            {
                String date = timeEntry.getAsOfDate();
                Map<String, Number> dateAttributeMap = resultMap.computeIfAbsent(date, k -> new TreeMap<>());
                dateAttributeMap.put(attributeName, timeEntry.getReportedValue());
            }
        }

        return resultMap;
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



    private List<TimeSeriesResult> filterResults(List<TimeSeriesResult> resultList, String prefix)
    {
        // to compare and contrast ....

//        List<TimeSeriesResult> filteredList = new ArrayList<>();
//        for (TimeSeriesResult listResult : resultList) {
//            if (listResult.getFieldType().startsWith(prefix)) {
//                filteredList.add(listResult);
//            }
//        }
//        return filteredList;


         return resultList.stream()
            .filter( result -> result.getFieldType().startsWith(prefix))
            .collect(Collectors.toList());
    }

}
