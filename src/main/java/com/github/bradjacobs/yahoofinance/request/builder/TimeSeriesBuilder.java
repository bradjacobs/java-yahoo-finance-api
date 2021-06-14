package com.github.bradjacobs.yahoofinance.request.builder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TimeSeriesBuilder extends BasePeriodRequestBuilder<TimeSeriesBuilder>
{
    private String ticker;

    // merge seems to 'mess up' results (or at least that is what is perceived)  leave always false for now.
    private boolean merge = false;
    private boolean padTimeSeries = true;

    private FieldBuilder fieldBuilder = new FieldBuilder();


    public TimeSeriesBuilder()
    {
    }

    public TimeSeriesBuilder withTicker(String ticker) {
        this.ticker = ticker;
        return this;
    }
//    public TimeSeriesBuilder withMerge(boolean merge) {
//        this.merge = merge;
//        return this;
//    }
    public TimeSeriesBuilder withPadTimeSeries(boolean padTimeSeries) {
        this.padTimeSeries = padTimeSeries;
        return this;
    }

    public TimeSeriesBuilder withStatement(StatementType... statements) {
        fieldBuilder.withStatement(statements);
        return this;
    }

    public TimeSeriesBuilder withAllStatements() {
        fieldBuilder.withAllStatements();
        return this;
    }

    public TimeSeriesBuilder withTimeFrame(TimeSeriesUnit... timeFrames) {
        fieldBuilder.withTimeframes(timeFrames);
        return this;
    }

    public TimeSeriesBuilder withAllTimeFrame() {
        fieldBuilder.withAllTimeframes();
        return this;
    }



    @Override
    protected String _getRequestTicker()
    {
        return this.ticker;
    }

    @Override
    protected TimeSeriesBuilder getThis()
    {
        return this;
    }

    @Override
    protected Map<String, String> _buildParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();

        if (this.startPeriod != null)
        {
            requestParamMap.put(ParamKeys.PERIOD1, this.startPeriod.toString());
            if (this.endPeriod != null) {
                requestParamMap.put(ParamKeys.PERIOD2, this.endPeriod.toString());
            }
        }
        requestParamMap.put(ParamKeys.MERGE, String.valueOf(this.merge));
        requestParamMap.put(ParamKeys.PAD_TIME_SERIES, String.valueOf(this.padTimeSeries));

        List<String> fieldList = fieldBuilder.build();
        if (fieldList != null && fieldList.size() > 0)
        {
            // NOTE... this string can be __HUGE__
            String fieldValueString = String.join(",", fieldList);
            requestParamMap.put(ParamKeys.TYPE, fieldValueString);
        }

        return requestParamMap;
    }

    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        return YahooEndpoint.TIMESERIES;
    }




    /**
     * Simple class to read in data stored in json file.
     *  (still tbd what to actually do with it)
     *
     * NOTE: really the data can be all generated at once on initialization (b/c it never changes)
     *   but not worried about those kind of scenarios at present.
     *
     * NOTE 2:
     *   original fields can be discovered by going here:   https://finance.yahoo.com/quote/AAPL
     *   then click on the following tabs and look at network traffic for 'timeseries'
     *   * Statistics
     *   * Financials -> Income Statement
     *   * Financials -> Balance Sheet
     *   * Financials -> Cash Flow
     *
     * __UPDATE__
     *   Note 2 above is incorrect (or rather incomplete).  Namely the list of fields you can get
     *   from looking at a particular ticker can vary.  (example:  financial stocks tend to have extra fields)
     *
     *   thus have created a separate file with an 'attempt' to keep the rare fields from the commons ones.
     *
     */
    private static class FieldBuilder
    {
        private static final String FILE_NAME = "timeseries_fundamental_types.json";

        // read in the resource file into a cached map
        private static final Map<String, List<String>> dataMap = readTimeseriesTypeMap();


        private Set<StatementType> statementTypes = new LinkedHashSet<>();

        private Set<TimeSeriesUnit> timeFrames = new LinkedHashSet<>();

        public FieldBuilder withStatement(StatementType ... statements) {
            if (statements != null) {
                statementTypes.addAll(Arrays.asList(statements));
            }
            else {
                statementTypes.clear();
            }
            return this;
        }
        public FieldBuilder withAllStatements() {
            return withStatement(StatementType.values());
        }

        public FieldBuilder withTimeframes(TimeSeriesUnit ... timeUnits) {
            if (timeUnits != null) {
                timeFrames.addAll(Arrays.asList(timeUnits));
            }
            else {
                timeFrames.clear();
            }
            return this;
        }
        public FieldBuilder withAllTimeframes() {
            return withTimeframes(TimeSeriesUnit.values());
        }

        public List<String> build() {
            if (statementTypes.isEmpty()) {
                withAllStatements();
            }
            if (timeFrames.isEmpty()) {
                withAllTimeframes();
            }

            List<String> resultList = new ArrayList<>();
            for (StatementType statementType : statementTypes) {

                List<String> rawFields = dataMap.get(statementType.getKey());

                for (TimeSeriesUnit timeFrame : timeFrames) {
                    if (timeFrame.equals(TimeSeriesUnit.TRAILING) && !statementType.isTtmAllowed()) {
                        continue;
                    }

                    String prefix = timeFrame.name().toLowerCase();
                    for (String field : rawFields) {

                        // discovered a weird bug that Yahoo wants a different case for EBITDA for annual/quarterly (vs trailing)
                        // todo: this is a _PERFECT_ example of a terrible HACK nested in middle of code that should be handled better
                        if (field.equals("EBITDA") && !timeFrame.equals(TimeSeriesUnit.TRAILING)) {
                            field = "Ebitda";
                        }

                        resultList.add(prefix + field);
                    }
                }
            }

            return resultList;
        }


        //////////////////////////////////////////////////

        private static Map<String,List<String>> readTimeseriesTypeMap()
        {
            String rawJson = ResourceUtil.readResourceFileAsString(FILE_NAME);
            return convertToMap(rawJson);
        }

        private static Map<String,List<String>> convertToMap(String json) {
            try {
                JsonMapper mapper = new JsonMapper();
                return mapper.readValue(json, new TypeReference<Map<String, List<String>>>() {});
            }
            catch (JsonProcessingException e) {
                throw new RuntimeException("Unable to convert json string to map of lists: " + e.getMessage(), e);
            }
        }
    }


}
