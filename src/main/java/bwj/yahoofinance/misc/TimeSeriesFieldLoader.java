/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.misc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang.StringUtils;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
public class TimeSeriesFieldLoader
{
    private static final String FILE_NAME = "timeseries_fundamental_types.json";
    private static final ObjectMapper mapper = new ObjectMapper();

    // eventually turn this into unit tests..
    public static void main(String[] args) {

        TimeSeriesFieldLoader loader = new TimeSeriesFieldLoader();

        List<String> allFields =
                new TimeSeriesFieldLoader.Builder().withAllStatements().withAllTimeframes().build();

        List<String> allAnnualFields =
                new TimeSeriesFieldLoader.Builder().withAllStatements().withTimeframes(TimeSeriesUnit.ANNUAL).build();

        List<String> ttmIncomeStatementAndValueFields =
                new TimeSeriesFieldLoader.Builder().withStatement(StatementType.INC_STMT, StatementType.VALUE).withTimeframes(TimeSeriesUnit.TRAILING).build();

        // this should be empty  (balance sheet values doen't support TTM)
        List<String> ttmBalanceSheetFields =
                new TimeSeriesFieldLoader.Builder().withStatement(StatementType.BAL_SHEET).withTimeframes(TimeSeriesUnit.TRAILING).build();

        List<String> ttmFields =
                new TimeSeriesFieldLoader.Builder().withTimeframes(TimeSeriesUnit.TRAILING).build();

        System.out.println("complete!");
    }

    private static final Map<String,List<String>> dataMap = readTimeseriesTypeMap();

    public TimeSeriesFieldLoader()
    {
    }


    public static class Builder
    {
        private Set<StatementType> statementTypes = new LinkedHashSet<>();
        private Set<TimeSeriesUnit> timeFrames = new LinkedHashSet<>();

        public Builder withStatement(StatementType ... statements) {
            if (statements != null) {
                statementTypes.addAll(Arrays.asList(statements));
            }
            return this;
        }
        public Builder withAllStatements() {
            return withStatement(StatementType.values());
        }
        public Builder withTimeframes(TimeSeriesUnit ... timeUnits) {
            if (timeUnits != null) {
                timeFrames.addAll(Arrays.asList(timeUnits));
            }
            return this;
        }
        public Builder withAllTimeframes() {
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
    }

    //////////////////////////////////////////////////

    private static Map<String,List<String>> readTimeseriesTypeMap()
    {
        String rawJson = readResourceFile(FILE_NAME);
        return convertToMap(rawJson);
    }

    private static Map<String,List<String>> convertToMap(String json) {
        if (StringUtils.isEmpty(json)) {
            return Collections.emptyMap();
        }

        try {
            return mapper.readValue(json, new TypeReference<Map<String, List<String>>>() {});
        }
        catch (JsonProcessingException e) {
            throw new RuntimeException("Unable to convert json string to map of maps: " + e.getMessage(), e);
        }
    }

    private static String readResourceFile(String fileName)
    {
        try {
            URL resource = TimeSeriesFieldLoader.class.getClassLoader().getResource(fileName);
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read test resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }
}
