/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static bwj.yahoofinance.YahooEndpointFlag.*;

/**
 * YahooEndpoints are an enumeration of the Yahoo! Finance API methods available.
 *
 * IMPORTANT NOTE:
 *   This is NOT an official list or documentation of the Yahoo! Finance API.
 *   Since no official documentation was found as of this writing, the endpoint values
 *   below were derived from ad hoc internet searching.
 *
 *   Therefore CANNOT GUARANTEE that this endpoint list is correct, complete, or will remain up-to-date.
 *
 *   Extra Info:
 *     There are some "Premium" endpoints that are also available form Yahoo!, which are NOT represented here.
 *
 *  Also the {@link bwj.yahoofinance.YahooModule} enums for QuoteSummary endpoint.
 */
public enum YahooEndpoint
{
    QUOTE_SUMMARY("quoteSummary", 10),

    QUOTE("quote", 7, FLAG_SUPPORT_MULTI_TICKERS),

    // Chart is really "History" (or Price History)
    //  Note:  version 9 of chart requires a 'crumb' but didn't notice any functional difference at initial glance.
    CHART("chart", 8),

    // Spark is really "History" but only will return a "close" and/or "adjclose"
    //  Note: spark v8 has a very different (flatter) format than spark v7
    SPARK("spark", 8, FLAG_SUPPORT_MULTI_TICKERS),

    VALIDATE("quote/validate", 6, FLAG_SUPPORT_MULTI_TICKERS),
    RECOMMENDATIONS_BY_SYMBOL("recommendationsbysymbol", 7),
    ESG_CHART("esgChart", 1, FLAG_REQUIRES_SYMBOL_PARAM),
    ESG_PEER_SCORES("esgPeerScores", 1, FLAG_REQUIRES_SYMBOL_PARAM),

    //   Query endpoints  (not ready yet)
    LOOKUP("lookup", 1, FLAG_IS_QUERY),
    LOOKUP_TOTALS("lookup/totals", 1, FLAG_IS_QUERY),
    //SCREENER("screener", 1, FLAG_IS_QUERY, FLAG_REQUIRES_CRUMB, FLAG_REQUIRES_POST),
    //VISUALIZATION("visualization", 1, FLAG_IS_QUERY, FLAG_REQUIRES_CRUMB, FLAG_REQUIRES_POST),


    TIMESERIES("timeseries", 1, "ws/fundamentals-timeseries/"),
    INSIGHTS("insights", 2, "ws/insights/", FLAG_REQUIRES_SYMBOL_PARAM),
    TECHNICAL_EVENTS("nonsubscriber/technicalevents", 1, "ws/market-analytics/", FLAG_REQUIRES_SYMBOL_PARAM),

    OPTIONS("options", 7),


    // note: QuoteType is virtually identical to ".../quoteSummary/____?modules=quoteType"
    QUOTE_TYPE("quoteType/", 1, FLAG_REQUIRES_SYMBOL_PARAM);  // trailing slash _IS_ required in this case)


    private final String name;
    private final int version;
    private final String pathPrefix;
    private final boolean supportsMultipleTickers;
    private final boolean requiresSymbolParam;
    private final boolean isQuery;

    YahooEndpoint(String name, int version, YahooEndpointFlag ... flags) {
        this(name, version, "", flags);
    }
    YahooEndpoint(String name, int version, String pathPrefix) {
        this(name, version, pathPrefix, new YahooEndpointFlag[0]);
    }
    YahooEndpoint(String name, int version, String pathPrefix, YahooEndpointFlag ... flags) {
        Set<YahooEndpointFlag> flagSet = new HashSet<>(Arrays.asList(flags));
        this.name = name;
        this.version = version;
        this.pathPrefix = pathPrefix;
        this.supportsMultipleTickers = flagSet.contains(FLAG_SUPPORT_MULTI_TICKERS);
        this.requiresSymbolParam = flagSet.contains(FLAG_REQUIRES_SYMBOL_PARAM);
        this.isQuery = flagSet.contains(FLAG_IS_QUERY);
    }


    public String getName() {
        return name;
    }

    public int getVersion() {
        return version;
    }

    public boolean getSupportsMultipleTickers() {
        return supportsMultipleTickers;
    }

    public boolean getRequiresSymbolParam() {
        return requiresSymbolParam;
    }

    public boolean getIsQuery() {
        return isQuery;
    }

    public String getPathPrefix() {
        return pathPrefix;
    }
}
