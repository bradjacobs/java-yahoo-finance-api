/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

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
 */
public enum YahooEndpoint
{
    OPTIONS("options", 7, false, false),

    QUOTE("quote", 7, false, true),

    // Chart is really "History" (or Price History)
    //  Note:  version 9 of chart requires a 'crumb' but didn't notice any functional difference at initial glance.
    CHART("chart", 8, false, false),

    // Spark is really "History" but only will return a "close" and/or "adjclose"
    //  Note: spark v8 has a very different (flatter) format than spark v7
    SPARK("spark", 8, false, true),

    VALIDATE("quote/validate", 6, false, true),
    RECOMMENDATIONS_BY_SYMBOL("recommendationsbysymbol", 7, false, false),
    ESG_CHART("esgChart", 1, false, false),
    ESG_PEER_SCORES("esgPeerScores", 1, false, false),


    // Below are the quoteSummary + modules choices

    // general info
    ASSET_PROFILE("assetProfile"),
    SUMMARY_PROFILE("summaryProfile"),
    SUMMARY_DETAIL("summaryDetail"),
    QUOTE_TYPE("quoteType"),

    // financial info
    BALANCE_SHEET_HISTORY("balanceSheetHistory"),
    BALANCE_SHEET_HISTORY_QUARTERLY("balanceSheetHistoryQuarterly"),
    DEFAULT_KEY_STATISTICS("defaultKeyStatistics"),
    CASH_FLOW_STMT_HISTORY("cashFlowStatementHistory"),
    CASJ_FLOW_STMT_HISTORY_QUARTERLY("cashFlowStatementHistoryQuarterly"),
    EARNINGS("earnings"),
    EARNINGS_HISTORY("earningsHistory"),
    FINANCIAL_DATA("financialData"),
    INCOME_STMT_HISTORY("incomeStatementHistory"),
    INCOME_STMT_HISTORY_QUARTERLY("incomeStatementHistoryQuarterly"),
    PRICE("price"),

    // (everything else)
    CALENDAR_EVENTS("calendarEvents"),
    EARNINGS_TREND("earningsTrend"),
    ESG_SCORES("esgScores"),
    FUND_OWNERSHIP("fundOwnership"),
    INDEX_TREND("indexTrend"),
    INDUSTRY_TREND("industryTrend"),
    INSIDER_HOLDERS("insiderHolders"),
    INSIDER_TRANSACTIONS("insiderTransactions"),
    INSTITUTION_OWNERSHIP("institutionOwnership"),
    MAJOR_HOLDERS("majorDirectHolders"),
    MAJOR_HOLDERS_BREAKDOWN("majorHoldersBreakdown"),
    NET_SHARE_PURCHASE_ACTIVITY("netSharePurchaseActivity"),
    PAGE_VIEWS("pageviews"),
    RECOMMENDATION_TREND("recommendationTrend"),
    SEC_FILLINGS("secFilings"),
    SECTOR_TREND("sectorTrend"),
    UPGRADE_DOWNGRADE_HISTORY("upgradeDowngradeHistory"),

    //  Fund-specific
    FUND_PERFORMANCE("fundPerformance"),
    FUND_PROFILE("fundProfile"),
    TOP_HOLDINGS("topHoldings");


    private static final int DEFAULT_VERSION = 10;
    private static final boolean DEFAULT_IS_QUOTE_SUMMARY = true;
    private static final boolean DEFAULT_SUPPORTS_MULTIPLE_TICKERS = false;

    private final String name;
    private final int version;
    private final String moduleName;
    private final boolean supportsMultipleTickers;

    private static final String QUOTE_SUMMARY = "quoteSummary";

    YahooEndpoint(String name)
    {
        this(name, DEFAULT_VERSION, DEFAULT_IS_QUOTE_SUMMARY, DEFAULT_SUPPORTS_MULTIPLE_TICKERS);
    }


    YahooEndpoint(String name, int version, boolean isQuoteSummaryModule, boolean supportsMultipleTickers)
    {
        if (isQuoteSummaryModule) {
            this.name = QUOTE_SUMMARY;
            this.moduleName = name;
        }
        else {
            this.name = name;
            this.moduleName = "";
        }
        this.version = version;
        this.supportsMultipleTickers = supportsMultipleTickers;
    }

    public String getName() {
        return name;
    }
    public String getModuleName() {
        return moduleName;
    }

    public int getVersion() {
        return version;
    }

    public boolean isQuoteSummaryModule() {
        return this.name.equals(QUOTE_SUMMARY);
    }

    public boolean isSupportsMultipleTickers() {
        return supportsMultipleTickers;
    }

    /*
       Skip this one for now b/c of special looking URL format
       https://query1.finance.yahoo.com/ws/insights/v2/finance/insights?symbol=AAPL



     */
}
