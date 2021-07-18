/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-07-18")
public enum ScreenerField
{
    // Basic Fields
    ALTMANZSCOREUSINGTHEAVERAGESTOCKINFORMATIONFORAPERIOD("altmanzscoreusingtheaveragestockinformationforaperiod.lasttwelvemonths", false, "Altman Z Score Using the Average Stock Information for a Period", false),
    AVGDAILYVOL3M("avgdailyvol3m", true, "Average Daily 3m Volume", false),
    BASICEPSCONTINUINGOPERATIONS("basicepscontinuingoperations.lasttwelvemonths", false, "Basic EPS - Continuing Operations", false),
    BETA("beta", true, "Beta", false),
    BOOKVALUESHARE("bookvalueshare.lasttwelvemonths", false, "Book Value / Share", false),
    CAPITALEXPENDITURE("capitalexpenditure.lasttwelvemonths", false, "Capital Expenditure", false),
    CASHFROMOPERATIONS("cashfromoperations.lasttwelvemonths", false, "Cash from Operations", false),
    CASHFROMOPERATIONS1YRGROWTH("cashfromoperations1yrgrowth.lasttwelvemonths", false, "Cash From Operations, 1 Yr. Growth %", false),
    CONSECUTIVE_YEARS_OF_DIVIDEND_GROWTH_COUNT("consecutive_years_of_dividend_growth_count", true, "Consecutive Years of Dividend Growth Count", false),
    CURRENTRATIO("currentratio.lasttwelvemonths", false, "Current Ratio", false),
    DAYS_TO_COVER_SHORT("days_to_cover_short.value", true, "Short Interest Ratio", false),
    DAYVOLUME("dayvolume", true, "Day Volume", false),
    DILUTEDEPS1YRGROWTH("dilutedeps1yrgrowth.lasttwelvemonths", false, "Diluted EPS, 1 Yr. Growth %", false),
    DILUTEDEPSCONTINUINGOPERATIONS("dilutedepscontinuingoperations.lasttwelvemonths", false, "Diluted EPS - Continuing Operations", false),
    EBIT("ebit.lasttwelvemonths", false, "EBIT", false),
    EBITDA("ebitda.lasttwelvemonths", false, "EBITDA", false),
    EBITDA1YRGROWTH("ebitda1yrgrowth.lasttwelvemonths", false, "EBITDA, 1 Yr. Growth %", false),
    EBITDAMARGIN("ebitdamargin.lasttwelvemonths", false, "EBITDA Margin %", false),
    ENVIRONMENTAL_SCORE("environmental_score", true, "Environmental Score", false),
    EODPRICE("eodprice", true, "EOD Price", false),
    EODVOLUME("eodvolume", true, "EOD Volume", false),
    EPSGROWTH("epsgrowth.lasttwelvemonths", false, "EPS Growth", false),
    ESG_SCORE("esg_score", true, "ESG Score", false),
    EXCHANGE("exchange", false, "Exchange", false),
    FIFTYTWOWKPERCENTCHANGE("fiftytwowkpercentchange", true, "52 Week Percent Change", false),
    FORWARD_DIVIDEND_PER_SHARE("forward_dividend_per_share", true, "Forward Dividend Per Share", false),
    FORWARD_DIVIDEND_YIELD("forward_dividend_yield", true, "Forward Dividend Yield", false),
    GOVERNANCE_SCORE("governance_score", true, "Governance Score", false),
    GROSSPROFIT("grossprofit.lasttwelvemonths", false, "Gross Profit", false),
    GROSSPROFITMARGIN("grossprofitmargin.lasttwelvemonths", false, "Gross Profit Margin %", false),
    HIGHEST_CONTROVERSY("highest_controversy", false, "Highest Controversy", false),
    INDUSTRY("industry", true, "Industry", false),
    INTRADAYMARKETCAP("intradaymarketcap", true, "Intraday Market Cap", false),
    INTRADAYPRICE("intradayprice", true, "Intraday Price", false),
    INTRADAYPRICECHANGE("intradaypricechange", true, "Change", false),
    LASTCLOSE52WEEKHIGH("lastclose52weekhigh.lasttwelvemonths", false, "Last Close 52 Week High", false),
    LASTCLOSE52WEEKLOW("lastclose52weeklow.lasttwelvemonths", false, "Last Close 52 Week Low", false),
    LASTCLOSEMARKETCAP("lastclosemarketcap.lasttwelvemonths", false, "Last Close Market Cap", false),
    LASTCLOSEMARKETCAPTOTALREVENUE("lastclosemarketcaptotalrevenue.lasttwelvemonths", false, "Last Close Market Cap / Total Revenue", false),
    LASTCLOSEPRICEBOOKVALUE("lastclosepricebookvalue.lasttwelvemonths", false, "Last Close Price / Book Value", false),
    LASTCLOSEPRICEEARNINGS("lastclosepriceearnings.lasttwelvemonths", false, "Last Close Price / Earnings", false),
    LASTCLOSEPRICETANGIBLEBOOKVALUE("lastclosepricetangiblebookvalue.lasttwelvemonths", false, "Last Close Price / Tangible Book Value", false),
    LASTCLOSETEVEBIT("lastclosetevebit.lasttwelvemonths", false, "Last Close TEV / EBIT", false),
    LASTCLOSETEVEBITDA("lastclosetevebitda.lasttwelvemonths", false, "Last Close TEV / EBITDA", false),
    LASTCLOSETEVTOTALREVENUE("lastclosetevtotalrevenue.lasttwelvemonths", false, "Last Close TEV / Total Revenue", false),
    LEVEREDFREECASHFLOW("leveredfreecashflow.lasttwelvemonths", false, "Levered Free Cash Flow", false),
    LEVEREDFREECASHFLOW1YRGROWTH("leveredfreecashflow1yrgrowth.lasttwelvemonths", false, "Levered Free Cash Flow, 1 Yr. Growth %", false),
    LTDEBTEQUITY("ltdebtequity.lasttwelvemonths", false, "LT Debt/Equity", false),
    NETDEBTEBITDA("netdebtebitda.lasttwelvemonths", false, "Net Debt / EBITDA", false),
    NETEPSBASIC("netepsbasic.lasttwelvemonths", false, "Net EPS - Basic", false),
    NETEPSDILUTED("netepsdiluted.lasttwelvemonths", false, "Net EPS - Diluted", false),
    NETINCOME1YRGROWTH("netincome1yrgrowth.lasttwelvemonths", false, "Net Income, 1 Yr. Growth %", false),
    NETINCOMEIS("netincomeis.lasttwelvemonths", false, "Net Income - (IS)", false),
    NETINCOMEMARGIN("netincomemargin.lasttwelvemonths", false, "Net Income Margin %", false),
    OPERATINGCASHFLOWTOCURRENTLIABILITIES("operatingcashflowtocurrentliabilities.lasttwelvemonths", false, "Operating Cash Flow to Current Liabilities", false),
    OPERATINGINCOME("operatingincome.lasttwelvemonths", false, "Operating Income", false),
    PCTHELDINSIDER("pctheldinsider", false, "Percent of shares held by insiders", false),
    PCTHELDINST("pctheldinst", false, "Percent of shares held by institutions", false),
    PEER_GROUP("peer_group", false, "Peer Group", false),
    PEGRATIO_5Y("pegratio_5y", false, "PEG Ratio (5 yr expected)", false),
    PERATIO("peratio.lasttwelvemonths", false, "Trailing P/E", false),
    PERCENTCHANGE("percentchange", true, "Percent Change", false),
    PRICEBOOKRATIO("pricebookratio.quarterly", false, "Price/Book", false),
    QUARTERLYREVENUEGROWTH("quarterlyrevenuegrowth.quarterly", false, "Quarterly Revenue Growth", false),
    QUICKRATIO("quickratio.lasttwelvemonths", false, "Quick Ratio", false),
    REGION("region", false, "Region", false),
    RETURNONASSETS("returnonassets.lasttwelvemonths", false, "Return on Assets", false),
    RETURNONEQUITY("returnonequity.lasttwelvemonths", false, "Return On Equity %", false),
    RETURNONTOTALCAPITAL("returnontotalcapital.lasttwelvemonths", true, "Return on Total Capital", false),
    SECTOR("sector", true, "Sector", false),
    SHORT_INTEREST("short_interest.value", true, "Short Interest", false),
    SHORT_INTEREST_PERCENTAGE_CHANGE("short_interest_percentage_change.value", true, "Short Interest % Change", false),
    SHORT_PERCENTAGE_OF_FLOAT("short_percentage_of_float.value", true, "Short % of Float", false),
    SHORT_PERCENTAGE_OF_SHARES_OUTSTANDING("short_percentage_of_shares_outstanding.value", true, "Short % of Shares Outstanding", false),
    SOCIAL_SCORE("social_score", true, "Social Score", false),
    TOTALASSETS("totalassets.lasttwelvemonths", false, "Total Assets", false),
    TOTALCASHANDSHORTTERMINVESTMENTS("totalcashandshortterminvestments.lasttwelvemonths", false, "Total Cash And Short Term Investments", false),
    TOTALCOMMONEQUITY("totalcommonequity.lasttwelvemonths", false, "Total Common Equity", false),
    TOTALCOMMONSHARESOUTSTANDING("totalcommonsharesoutstanding.lasttwelvemonths", false, "Total Common Shares Outstanding", false),
    TOTALCURRENTASSETS("totalcurrentassets.lasttwelvemonths", false, "Total Current Assets", false),
    TOTALCURRENTLIABILITIES("totalcurrentliabilities.lasttwelvemonths", false, "Total Current Liabilities", false),
    TOTALDEBT("totaldebt.lasttwelvemonths", false, "Total Debt", false),
    TOTALDEBTEBITDA("totaldebtebitda.lasttwelvemonths", false, "Total Debt / EBITDA", false),
    TOTALDEBTEQUITY("totaldebtequity.lasttwelvemonths", false, "Total Debt/Equity", false),
    TOTALEQUITY("totalequity.lasttwelvemonths", false, "Total Equity", false),
    TOTALREVENUES("totalrevenues.lasttwelvemonths", false, "Total Revenues", false),
    TOTALREVENUES1YRGROWTH("totalrevenues1yrgrowth.lasttwelvemonths", false, "Total Revenues, 1 Yr. Growth %", false),
    TOTALSHARESOUTSTANDING("totalsharesoutstanding", false, "Total Shares Outstanding", false),
    UNLEVEREDFREECASHFLOW("unleveredfreecashflow.lasttwelvemonths", false, "Unlevered Free Cash Flow", false),
    
    // Premium Fields
    EARNINGS_CONSISTENCY("earnings_consistency", false, "Earnings Consistency", true),
    ESTIMATED_EARNINGS_GROWTH("estimated_earnings_growth", true, "Estimated Earnings Growth YoY %", true),
    ESTIMATED_REVENUE_GROWTH("estimated_revenue_growth", true, "Estimated Revenue Growth YoY %", true),
    REVENUE_CONSISTENCY("revenue_consistency", false, "Revenue Consistency", true),
    ROR_PERCENT("ror_percent", true, "Rate of Return", true),
    VALUE_DESCRIPTION("value_description", false, "Value Description", true),
    YEARS_OF_CONSECUTIVE_POSITIVE_EPS("years_of_consecutive_positive_eps", true, "Years of Consecutive Positive EPS", true);



    private final String value;
    private final boolean sortable;
    private final String displayString;
    private final boolean isPremium;

    ScreenerField(String value, boolean sortable, String displayString, boolean isPremium) {
        this.value = value;
        this.sortable = sortable;
        this.displayString = displayString;
        this.isPremium = isPremium;
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

    public boolean isPremium() {
        return isPremium;
    }
}
