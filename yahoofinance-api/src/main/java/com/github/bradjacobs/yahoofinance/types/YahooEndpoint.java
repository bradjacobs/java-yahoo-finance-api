/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static com.github.bradjacobs.yahoofinance.types.YahooEndpointFlag.*;

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
 *
 * NOT INCLUDED:
 *  - "Premium" endpoints
 *  - "portfolio" endpoints
 *  - "screener specific" endpoints (saved/predefined screens, etc)
 *  - "video" endpoints
 *  - anything that requires username/userId/login
 *  - other misc endpoints that were subjectively determined not to have much usefulness here.
 *
 *
 *  Also the {@link YahooModule} types for QuoteSummary endpoint.
 */
public enum YahooEndpoint
{
    //   NOTE: see YahooModule for relevant modules
    //   NOTE:  "&formatted=false" stops working on v11
    QUOTE_SUMMARY("quoteSummary", 10),

    QUOTE("quote", 7, FLAG_SUPPORT_MULTI_TICKERS),

    // Price History
    //  NOTE: version 9 requires a 'crumb' but didn't notice any functional difference.
    CHART("chart", 8),

    // Price History (alternate)
    //   NOTE:  primary differences from CHART
    //     1.  only returns "close" (and timestamps)
    //     2.  will support multiple tickers
    //     3.  response will have a different format
    //   NOTE: noticeable difference in response format b/w v8 and v7
    SPARK("spark", 8, FLAG_SUPPORT_MULTI_TICKERS),


    VALIDATE("quote/validate", 6, FLAG_SUPPORT_MULTI_TICKERS),
    RECOMMENDATIONS_BY_SYMBOL("recommendationsbysymbol", 7),
    ESG_CHART("esgChart", 1, FLAG_REQUIRES_SYMBOL_PARAM),
    ESG_PEER_SCORES("esgPeerScores", 1, FLAG_REQUIRES_SYMBOL_PARAM),


    // Query endpoints  (NOT READY!)
    LOOKUP("lookup", 1, FLAG_IS_QUERY),
    LOOKUP_TOTALS("lookup/totals", 1, FLAG_IS_QUERY),
    SCREENER("screener", 1, FLAG_IS_QUERY, FLAG_REQUIRES_CRUMB, FLAG_REQUIRES_POST),
    SCREENER_TOTALS("screener/total", 1, FLAG_IS_QUERY, FLAG_REQUIRES_CRUMB, FLAG_REQUIRES_POST),

    //  NOTE: 'visualization' correlates to the information that can be viewed at:  https://finance.yahoo.com/calendar
    //     implementing the functionality for this is currently a lower priority.
    //VISUALIZATION("visualization", 1, FLAG_IS_QUERY, FLAG_REQUIRES_CRUMB, FLAG_REQUIRES_POST),

    //SEARCH("search", 1, FLAG_IS_QUERY),   // commented out b/c how it should work is semi-mysterious


    TIMESERIES("timeseries", 1, "ws/fundamentals-timeseries/"),
    INSIGHTS("insights", 2, "ws/insights/", FLAG_REQUIRES_SYMBOL_PARAM),
    TECHNICAL_EVENTS("nonsubscriber/technicalevents", 1, "ws/market-analytics/", FLAG_REQUIRES_SYMBOL_PARAM),

    OPTIONS("options", 7),


    //  NOTE:  virtually identical to ".../quoteSummary/{symbol}?modules=quoteType"
    QUOTE_TYPE("quoteType", 1),


    //  Regional requests
    MARKET_SUMMARY("quote/marketSummary", 6, FLAG_IS_REGION),
    TRENDING("trending/", 1, FLAG_IS_REGION), // 'trending/US' or 'trending/?region=US'



    // NOTE: ----
    //    intro attempt of supporting 'premium' endpoints.  Must have a valid Yahoo username/password
    //      and be signed up for the premium service.

    PREMIUM_TIMESERIES("premium/timeseries", 1, "ws/fundamentals-timeseries/", FLAG_IS_PREMIUM);





    //   NOTE: allegedly this is valid endpoint, but never seen it work
    //NEWS("news", 2, FLAG_SUPPORT_MULTI_TICKERS),


    private final String name;
    private final int version;
    private final String pathPrefix;

    private final Set<YahooEndpointFlag> flags;

    YahooEndpoint(String name, int version, YahooEndpointFlag... flags) {
        this(name, version, "", flags);
    }
    YahooEndpoint(String name, int version, String pathPrefix) {
        this(name, version, pathPrefix, new YahooEndpointFlag[0]);
    }
    YahooEndpoint(String name, int version, String pathPrefix, YahooEndpointFlag ... flags) {
        this.flags = new HashSet<>(Arrays.asList(flags));
        this.name = name;
        this.version = version;
        this.pathPrefix = pathPrefix;
    }


    public String getName() {
        return name;
    }

    public int getVersion() {
        return version;
    }

    public String getPathPrefix() {
        return pathPrefix;
    }

    public boolean isMultiTickerSupported() {
        return flags.contains(FLAG_SUPPORT_MULTI_TICKERS);
    }

    public boolean isQuery() {
        return flags.contains(FLAG_IS_QUERY);
    }

    public boolean isCrumbRequest() {
        return flags.contains(FLAG_REQUIRES_CRUMB);
    }

    public boolean isPostRequest() {
        return flags.contains(FLAG_REQUIRES_POST);
    }

    public boolean isTickerKeyValueParam() {
        return flags.contains(FLAG_REQUIRES_SYMBOL_PARAM);
    }

    private boolean isRegionRequest() {
        return flags.contains(FLAG_IS_REGION);
    }

    public boolean isPremiumRequest() {
        return flags.contains(FLAG_IS_PREMIUM);
    }


    /**
     * Check if endpoint request contains ticker as part of the URL path
     *   (as opposed to a key/value) parameter
     *   i.e.
     *       ../quoteSummary/AAPL?modules=financialData (returns TRUE)
     *       ../quote?symbols=AAPL (returns FALSE)
     * @return if ticker on the path portion of a URL request.
     */
    public boolean isTickerOnPath() {
        if (isQuery()) { return false; }
        if (isMultiTickerSupported()) { return false; }
        if (isTickerKeyValueParam()) { return false; }
        if (isRegionRequest()) { return false; }

        return true;
    }

    public boolean requiresTicker() {
        if (isQuery()) { return false; }
        if (isRegionRequest()) { return false; }
        return true;
    }

}
