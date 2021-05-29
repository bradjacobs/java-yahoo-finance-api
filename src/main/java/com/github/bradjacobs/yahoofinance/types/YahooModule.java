/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

/**
 * Module choices when using the "QuoteSummary" endpoint
 */
public enum YahooModule
{
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
    FINANCIALS_TEMPLATE("financialsTemplate"), // note: doesn't seem too useful upon initial glance
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
    TOP_HOLDINGS("topHoldings"),

    // Futures-specific
    FUTURES_CHAIN("futuresChain"),

    // Misc
    COMPONENTS("components");  // only applies for limited items (like "^DJI")



    private final String name;

    YahooModule(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
