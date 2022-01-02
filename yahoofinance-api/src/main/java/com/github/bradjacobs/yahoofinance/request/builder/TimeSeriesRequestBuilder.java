package com.github.bradjacobs.yahoofinance.request.builder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.types.StatementType;
import com.github.bradjacobs.yahoofinance.types.TimeSeriesUnit;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TimeSeriesRequestBuilder extends BasePeriodRequestBuilder<TimeSeriesRequestBuilder>
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();


    private String ticker;

    // merge seems to 'mess up' results (or at least that is what is perceived)  leave always false for now.
    private boolean merge = false;
    private boolean padTimeSeries = true;

    private boolean usePremium = false;

    private final FieldBuilder fieldBuilder = new FieldBuilder();

    @Override
    protected List<String> getRequiredParameters() {
        return Arrays.asList(ParamKeys.PERIOD1, ParamKeys.PERIOD2);
    }

    public TimeSeriesRequestBuilder() { }

    public TimeSeriesRequestBuilder withTicker(String ticker) {
        this.ticker = ticker;
        return this;
    }

    public TimeSeriesRequestBuilder withPadTimeSeries(boolean padTimeSeries) {
        this.padTimeSeries = padTimeSeries;
        return this;
    }

    public TimeSeriesRequestBuilder withStatement(StatementType... statements) {
        fieldBuilder.withStatement(statements);
        return this;
    }

    public TimeSeriesRequestBuilder withAllStatements() {
        fieldBuilder.withAllStatements();
        return this;
    }

    public TimeSeriesRequestBuilder withTimeFrame(TimeSeriesUnit... timeFrames) {
        fieldBuilder.withTimeframes(timeFrames);
        return this;
    }

    public TimeSeriesRequestBuilder withAllTimeFrame() {
        fieldBuilder.withAllTimeframes();
        return this;
    }

    public TimeSeriesRequestBuilder withCustomFields(String ... fieldNames) {
        fieldBuilder.withCustomFields(fieldNames);
        return this;
    }

    public TimeSeriesRequestBuilder setPremium(boolean premium) {
        this.usePremium = premium;
        return this;
    }

    @Override
    protected String getRequestTicker()
    {
        return this.ticker;
    }

    @Override
    protected TimeSeriesRequestBuilder getThis()
    {
        return this;
    }

    @Override
    protected Map<String, String> buildEndpointParamMap()
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
        if (fieldList.size() > 0)
        {
            // NOTE... this string can be __HUGE__
            String fieldValueString = String.join(",", fieldList);
            requestParamMap.put(ParamKeys.TYPE, fieldValueString);
        }

        return requestParamMap;
    }

    @Override
    protected YahooEndpoint getEndpoint()
    {
        if (this.usePremium) {
            return YahooEndpoint.PREMIUM_TIMESERIES;
        }
        else {
            return YahooEndpoint.TIMESERIES;
        }
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


        private final Set<StatementType> statementTypes = new LinkedHashSet<>();

        private final Set<TimeSeriesUnit> timeFrames = new LinkedHashSet<>();

        private final Set<String> customFields = new LinkedHashSet<>();


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

        public FieldBuilder withCustomFields(String ... fieldNames) {
            if (fieldNames != null && fieldNames.length > 0) {
                customFields.addAll(Arrays.asList(fieldNames));
            }
            return this;
        }

        public List<String> build() {
            if (statementTypes.isEmpty()) {
                withAllStatements();
            }
            if (timeFrames.isEmpty()) {
                withAllTimeframes();
            }

            if (customFields.size() > 0) {
                return buildCustomFields();
            }

            List<String> resultList = new ArrayList<>();
            for (StatementType statementType : statementTypes) {

                List<String> rawFields = dataMap.get(statementType.getKey());

                for (TimeSeriesUnit timeFrame : timeFrames) {
                    if (timeFrame.equals(TimeSeriesUnit.TRAILING) && !statementType.isTtmAllowed()) {
                        continue;
                    }

                    for (String field : rawFields) {
                        //  note: nested method causes redundant calls of 'timeFrame.name().toLowerCase()', but presently low priority concern.
                        resultList.add( constructFullFieldName(timeFrame, field) );
                    }
                }
            }

            return resultList;
        }


        private List<String> buildCustomFields()
        {
            List<String> resultList = new ArrayList<>();

            for (String customField : customFields)
            {
                if (hasTimePrefix(customField)) {
                    resultList.add(customField);
                }
                else
                {
                    for (TimeSeriesUnit timeFrame : timeFrames) {
                        resultList.add( constructFullFieldName(timeFrame, customField) );
                    }
                }
            }

            return resultList;
        }

        private String constructFullFieldName(TimeSeriesUnit timeFrame, String baseFieldName)
        {
            String prefix = timeFrame.name().toLowerCase();

            // discovered a weird bug that Yahoo wants a different case for EBITDA for annual/quarterly (vs trailing)
            // todo: this is a _PERFECT_ example of a terrible HACK nested in middle of code that should be handled better
            if (baseFieldName.equals("EBITDA") && !timeFrame.equals(TimeSeriesUnit.TRAILING))
            {
                baseFieldName = "Ebitda";
            }

            return prefix + baseFieldName.substring(0, 1).toUpperCase() + baseFieldName.substring(1);
        }


        private boolean hasTimePrefix(String fieldName)
        {
            for (TimeSeriesUnit timeSeriesUnit : TimeSeriesUnit.values()) {
                String prefix = timeSeriesUnit.name().toLowerCase();
                if (fieldName.startsWith(prefix)) {
                    return true;
                }
            }
            return false;
        }




        //////////////////////////////////////////////////

        private static Map<String,List<String>> readTimeseriesTypeMap()
        {
            String rawJson = ResourceUtil.readResourceFileAsString(FILE_NAME);
            return convertToMap(rawJson);
        }

        private static Map<String,List<String>> convertToMap(String json) {
            try {
                return mapper.readValue(json, new TypeReference<Map<String, List<String>>>() {});
            }
            catch (JsonProcessingException e) {
                throw new RuntimeException("Unable to convert json string to map of lists: " + e.getMessage(), e);
            }
        }
    }


}
