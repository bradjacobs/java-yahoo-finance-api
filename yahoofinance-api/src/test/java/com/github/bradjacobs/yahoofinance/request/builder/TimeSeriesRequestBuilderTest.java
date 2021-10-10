package com.github.bradjacobs.yahoofinance.request.builder;

public class TimeSeriesRequestBuilderTest
{

    // todo - implement

    // reference for later
    /*
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

     */
}
