/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

/**
 * Constants for url parameter "keys"
 *
 *  IMPORTANT: for any given endpoint only a subset of the values below will be applicable.
 */
public final class ParamKeys
{
    private ParamKeys() { }

    public static final String LANG = "lang";
    public static final String REGION = "region";
    public static final String CORSDOMAIN = "corsDomain";

    public static final String CRUMB = "crumb";

    public static final String SYMBOL = "symbol";
    public static final String SYMBOLS = "symbols";

    public static final String FORMATTED = "formatted";
    public static final String TYPE = "type";
    public static final String FIELDS = "fields";

    // for QuoteSummary
    public static final String MODULES = "modules";

    // typically for Queries
    public static final String Q = "q";
    public static final String QUERY = "query";
    public static final String START = "start";
    public static final String COUNT = "count";

    // typically for PriceHistory
    public static final String RANGE = "range";
    public static final String PERIOD1 = "period1";
    public static final String PERIOD2 = "period2";
    public static final String INTERVAL = "interval";
    public static final String INCLUDE_ADJ_CLOSE = "includeAdjustedClose";
    public static final String EVENTS = "events";
    public static final String INDICATORS = "indicators";
    public static final String INCLUDE_TIMESTAMPS = "includeTimestamps";
    public static final String INCLUDE_PRE_POST = "includePrePost";
    public static final String KEY_NUMBER_OF_POINTS = "numberOfPoints";  // still tbd what this does
    public static final String KEY_USE_YFID = "useYfid";  // still tdb what this does


    // typically for TimeSeries
    public static final String MERGE = "merge";
    public static final String PAD_TIME_SERIES = "padTimeSeries";

    // typically for Screener
    public static final String USE_RECORD_RESPONSE = "useRecordResponse";  // only returns total (no records) when true


}
