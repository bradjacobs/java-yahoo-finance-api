/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-11-05")
public enum VisualizationField
{
    // Common
    COUNT("count", true, "Document Count"),
    EVENTID("eventid", false, "Event ID"),
    STARTDATETIME("startdatetime", true, "Event Time"),
    TICKER("ticker", false, "Tickers"),
    // Earnings
    DATEISESTIMATE("dateisestimate", false, "Estimate Date"),
    ENDDATETIME("enddatetime", true, "Event End Date"),
    EPSACTUAL("epsactual", true, "Reported EPS"),
    EPSCONSENSUS("epsconsensus", true, "EPS Consensus"),
    EPSESTIMATE("epsestimate", true, "EPS Estimate"),
    EPSSURPRISEPCT("epssurprisepct", true, "Surprise (%)"),
    EVENTNAME("eventname", false, "Event Name"),
    EVENTTYPE("eventtype", false, "Event Type"),
    FISCALYEAR("fiscalyear", false, "Financial Calendar Year"),
    QUARTER("quarter", false, "Quarter"),
    STARTDATETIMETYPE("startdatetimetype", false, "Event Start Time"),
    // Econ
    AFTER_RELEASE_ACTUAL("after_release_actual", false, "Actual"),
    CONSENSUS_ESTIMATE("consensus_estimate", false, "Market Expectation"),
    COUNTRY_CODE("country_code", true, "Country Code"),
    ECON_RELEASE("econ_release", false, "Event"),
    ORIGINALLY_REPORTED_ACTUAL("originally_reported_actual", false, "Revised from"),
    PERIOD("period", false, "For"),
    PRIOR_RELEASE_ACTUAL("prior_release_actual", false, "Prior to This"),
    RIC("ric", false, "RIC"),
    // IPO
    AMENDEDDATE("amendeddate", true, "Amended Date"),
    CURRENCYNAME("currencyname", false, "Currency"),
    DEALNO("dealno", false, "Deal ID"),
    DEALTYPE("dealtype", false, "Action"),
    EXCHANGE("exchange", false, "Exchange"),
    EXCHANGE_SHORT_NAME("exchange_short_name", false, "Exchange Short Name"),
    FILINGDATE("filingdate", true, "Filing Date"),
    INVESTORTYPE("investortype", false, "Investor Type"),
    OFFERPRICE("offerprice", true, "Price"),
    PRICEFROM("pricefrom", true, "Price From"),
    PRICETO("priceto", true, "Price To"),
    QUOTETYPE("quotetype", false, "Quote Type"),
    SHARES("shares", true, "Shares"),
    // Splits
    OLD_SHARE_WORTH("old_share_worth", false, "Old Share Worth"),
    OPTIONABLE("optionable", false, "Optionable?"),
    SHARE_WORTH("share_worth", false, "Share Worth"),
    // Research
    ABSTRACT("abstract", false, "Abstract"),
    AUTHOR("author", false, "Author"),
    COMPANY_NAME("company_name", false, "Company Name"),
    COMPANY_SUMMARY("company_summary", false, "Company Summary"),
    EPS_STATUS("eps_status", false, "Earnings Estimates"),
    ID("id", false, "ID"),
    INVESTMENT_RATING("investment_rating", false, "Rating"),
    INVESTMENT_RATING_STATUS("investment_rating_status", false, "Rating Status"),
    MARKET_MOVER("market_mover.section_text", false, "Market Mover's Section Text"),
    PDF_URL("pdf_url", false, "Pdf URL"),
    PROVIDER("provider", false, "Provider"),
    REPORT_DATE("report_date", true, "Report Date"),
    REPORT_HEADLINE("report_headline", false, "Report Headline"),
    REPORT_TITLE("report_title", false, "Header"),
    REPORT_TYPE("report_type", false, "Report type"),
    SECTOR("sector", false, "Sector"),
    SNAPSHOT_URL("snapshot_url", false, "Snapshot URL"),
    TARGET_PRICE("target_price", false, "Target Price"),
    TARGET_PRICE_STATUS("target_price_status", false, "Target Price Status");



    private final String value;
    private final boolean sortable;
    private final String displayString;

    VisualizationField(String value, boolean sortable, String displayString) {
        this.value = value;
        this.sortable = sortable;
        this.displayString = displayString;
    }

    public String getValue() {
        return value;
    }

    public boolean isSortable() {
        return sortable;
    }

    public String getDisplayString() {
        return displayString;
    }
}
