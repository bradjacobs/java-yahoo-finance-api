/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

/**
 * YahooEndpoints are an enumeration of the Yahoo! Finance API methods available.
 *
 * IMPORTANT NOTE:
 *   This is NOT an official list or documenation of the Yahoo! Finance API.
 *   Since no official documentation was found as of this writing, the endpoint values
 *   below were derived from ad hoc internet searching.
 *
 *   Therefore CANNOT GUARANTEE that this endpoint list is correct, complete, or will remain up-to-date.
 */
public enum YahooEndpoint
{
    OPTIONS("options", 7),
    CHART("chart", 8),  // commonly used for price history.

    // *** v10 ***  (quoteSummary / modules)
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
    FUND_OWNERSHIP("fundOwnership"),
    INDEX_TREND("indexTrend"),
    INDUSTRY_TREND("industryTrend"),
    INSIDER_HOLDERS("insiderHolders"),
    INSIDER_TRANSACTIONS("insiderTransactions"),
    INSTITUTION_OWNERSHIP("institutionOwnership"),
    MAJOR_HOLDERS("majorDirectHolders"),
    MAJOR_HOLDERS_BREAKDOWN("majorHoldersBreakdown"),
    NET_SHARE_PURCHASE_ACTIVITY("netSharePurchaseActivity"),
    RECOMMENDATION_TREND("recommendationTrend"),
    SEC_FILLINGS("secFilings"),
    SECTOR_TREND("sectorTrend"),
    UPGRADE_DOWNGRADE_HISTORY("upgradeDowngradeHistory"),

    //  Fund-specific
    FUND_PERFORMANCE("fundPerformance"),
    FUND_PROFILE("fundProfile"),
    TOP_HOLDINGS("topHoldings");


    private static final int DEFAULT_VERSION = 10;

    private final String path;
    private final String name;
    private final int ver;
    private final boolean isQuoteSummaryModule;

    YahooEndpoint(String name)
    {
        this(name, DEFAULT_VERSION);
    }

    YahooEndpoint(String name, int version)
    {
        this.name = name;
        this.ver = version;
        this.isQuoteSummaryModule = (!name.equals("chart") && !name.equals("options"));

        // path to be deprecated.
        if (name.equals("chart")) {
            this.path = "v8/finance/chart";
        }
        else if (name.equals("options")) {
            this.path = "v7/finance/options";
        }
        else {
            this.path = "v10/finance/quoteSummary/%s?modules=" + name;
        }
    }

    public String getPath() {
        return this.path;
    }

}
