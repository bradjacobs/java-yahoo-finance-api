/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-06-19")
public enum ScreenerField
{
    ALTMANZSCOREUSINGTHEAVERAGESTOCKINFORMATIONFORAPERIOD("altmanzscoreusingtheaveragestockinformationforaperiod.lasttwelvemonths", false, "Altman Z Score Using the Average Stock Information for a Period"),
    AVGDAILYVOL3M("avgdailyvol3m", true, "Average Daily 3m Volume"),
    BASICEPSCONTINUINGOPERATIONS("basicepscontinuingoperations.lasttwelvemonths", false, "Basic EPS - Continuing Operations"),
    BETA("beta", true, "Beta"),
    BOOKVALUESHARE("bookvalueshare.lasttwelvemonths", false, "Book Value / Share"),
    CAPITALEXPENDITURE("capitalexpenditure.lasttwelvemonths", false, "Capital Expenditure"),
    CASHFROMOPERATIONS("cashfromoperations.lasttwelvemonths", false, "Cash from Operations"),
    CASHFROMOPERATIONS1YRGROWTH("cashfromoperations1yrgrowth.lasttwelvemonths", false, "Cash From Operations, 1 Yr. Growth %"),
    CONSECUTIVE_YEARS_OF_DIVIDEND_GROWTH_COUNT("consecutive_years_of_dividend_growth_count", true, "Consecutive Years of Dividend Growth Count"),
    CURRENTRATIO("currentratio.lasttwelvemonths", false, "Current Ratio"),
    DAYS_TO_COVER_SHORT("days_to_cover_short.value", true, "Short Interest Ratio"),
    DAYVOLUME("dayvolume", true, "Day Volume"),
    DILUTEDEPS1YRGROWTH("dilutedeps1yrgrowth.lasttwelvemonths", false, "Diluted EPS, 1 Yr. Growth %"),
    DILUTEDEPSCONTINUINGOPERATIONS("dilutedepscontinuingoperations.lasttwelvemonths", false, "Diluted EPS - Continuing Operations"),
    EBIT("ebit.lasttwelvemonths", false, "EBIT"),
    EBITDA("ebitda.lasttwelvemonths", false, "EBITDA"),
    EBITDA1YRGROWTH("ebitda1yrgrowth.lasttwelvemonths", false, "EBITDA, 1 Yr. Growth %"),
    EBITDAMARGIN("ebitdamargin.lasttwelvemonths", false, "EBITDA Margin %"),
    ENVIRONMENTAL_SCORE("environmental_score", true, "Environmental Score"),
    EODPRICE("eodprice", true, "EOD Price"),
    EODVOLUME("eodvolume", true, "EOD Volume"),
    EPSGROWTH("epsgrowth.lasttwelvemonths", false, "EPS Growth"),
    ESG_SCORE("esg_score", true, "ESG Score"),
    EXCHANGE("exchange", false, "Exchange"),
    FIFTYTWOWKPERCENTCHANGE("fiftytwowkpercentchange", true, "52 Week Percent Change"),
    FORWARD_DIVIDEND_PER_SHARE("forward_dividend_per_share", true, "Forward Dividend Per Share"),
    FORWARD_DIVIDEND_YIELD("forward_dividend_yield", true, "Forward Dividend Yield"),
    GOVERNANCE_SCORE("governance_score", true, "Governance Score"),
    GROSSPROFIT("grossprofit.lasttwelvemonths", false, "Gross Profit"),
    GROSSPROFITMARGIN("grossprofitmargin.lasttwelvemonths", false, "Gross Profit Margin %"),
    HIGHEST_CONTROVERSY("highest_controversy", false, "Highest Controversy"),
    INDUSTRY("industry", true, "Industry"),
    INTRADAYMARKETCAP("intradaymarketcap", true, "Intraday Market Cap"),
    INTRADAYPRICE("intradayprice", true, "Intraday Price"),
    INTRADAYPRICECHANGE("intradaypricechange", true, "Change"),
    LASTCLOSE52WEEKHIGH("lastclose52weekhigh.lasttwelvemonths", false, "Last Close 52 Week High"),
    LASTCLOSE52WEEKLOW("lastclose52weeklow.lasttwelvemonths", false, "Last Close 52 Week Low"),
    LASTCLOSEMARKETCAP("lastclosemarketcap.lasttwelvemonths", false, "Last Close Market Cap"),
    LASTCLOSEMARKETCAPTOTALREVENUE("lastclosemarketcaptotalrevenue.lasttwelvemonths", false, "Last Close Market Cap / Total Revenue"),
    LASTCLOSEPRICEBOOKVALUE("lastclosepricebookvalue.lasttwelvemonths", false, "Last Close Price / Book Value"),
    LASTCLOSEPRICEEARNINGS("lastclosepriceearnings.lasttwelvemonths", false, "Last Close Price / Earnings"),
    LASTCLOSEPRICETANGIBLEBOOKVALUE("lastclosepricetangiblebookvalue.lasttwelvemonths", false, "Last Close Price / Tangible Book Value"),
    LASTCLOSETEVEBIT("lastclosetevebit.lasttwelvemonths", false, "Last Close TEV / EBIT"),
    LASTCLOSETEVEBITDA("lastclosetevebitda.lasttwelvemonths", false, "Last Close TEV / EBITDA"),
    LASTCLOSETEVTOTALREVENUE("lastclosetevtotalrevenue.lasttwelvemonths", false, "Last Close TEV / Total Revenue"),
    LEVEREDFREECASHFLOW("leveredfreecashflow.lasttwelvemonths", false, "Levered Free Cash Flow"),
    LEVEREDFREECASHFLOW1YRGROWTH("leveredfreecashflow1yrgrowth.lasttwelvemonths", false, "Levered Free Cash Flow, 1 Yr. Growth %"),
    LTDEBTEQUITY("ltdebtequity.lasttwelvemonths", false, "LT Debt/Equity"),
    NETDEBTEBITDA("netdebtebitda.lasttwelvemonths", false, "Net Debt / EBITDA"),
    NETEPSBASIC("netepsbasic.lasttwelvemonths", false, "Net EPS - Basic"),
    NETEPSDILUTED("netepsdiluted.lasttwelvemonths", false, "Net EPS - Diluted"),
    NETINCOME1YRGROWTH("netincome1yrgrowth.lasttwelvemonths", false, "Net Income, 1 Yr. Growth %"),
    NETINCOMEIS("netincomeis.lasttwelvemonths", false, "Net Income - (IS)"),
    NETINCOMEMARGIN("netincomemargin.lasttwelvemonths", false, "Net Income Margin %"),
    OPERATINGCASHFLOWTOCURRENTLIABILITIES("operatingcashflowtocurrentliabilities.lasttwelvemonths", false, "Operating Cash Flow to Current Liabilities"),
    OPERATINGINCOME("operatingincome.lasttwelvemonths", false, "Operating Income"),
    PCTHELDINSIDER("pctheldinsider", false, "Percent of shares held by insiders"),
    PCTHELDINST("pctheldinst", false, "Percent of shares held by institutions"),
    PEER_GROUP("peer_group", false, "Peer Group"),
    PEGRATIO_5Y("pegratio_5y", false, "PEG Ratio (5 yr expected)"),
    PERATIO("peratio.lasttwelvemonths", false, "Trailing P/E"),
    PERCENTCHANGE("percentchange", true, "Percent Change"),
    PRICEBOOKRATIO("pricebookratio.quarterly", false, "Price/Book"),
    QUARTERLYREVENUEGROWTH("quarterlyrevenuegrowth.quarterly", false, "Quarterly Revenue Growth"),
    QUICKRATIO("quickratio.lasttwelvemonths", false, "Quick Ratio"),
    REGION("region", false, "Region"),
    RETURNONASSETS("returnonassets.lasttwelvemonths", false, "Return on Assets"),
    RETURNONEQUITY("returnonequity.lasttwelvemonths", false, "Return On Equity %"),
    RETURNONTOTALCAPITAL("returnontotalcapital.lasttwelvemonths", true, "Return on Total Capital"),
    SECTOR("sector", true, "Sector"),
    SHORT_INTEREST("short_interest.value", true, "Short Interest"),
    SHORT_INTEREST_PERCENTAGE_CHANGE("short_interest_percentage_change.value", true, "Short Interest % Change"),
    SHORT_PERCENTAGE_OF_FLOAT("short_percentage_of_float.value", true, "Short % of Float"),
    SHORT_PERCENTAGE_OF_SHARES_OUTSTANDING("short_percentage_of_shares_outstanding.value", true, "Short % of Shares Outstanding"),
    SOCIAL_SCORE("social_score", true, "Social Score"),
    TOTALASSETS("totalassets.lasttwelvemonths", false, "Total Assets"),
    TOTALCASHANDSHORTTERMINVESTMENTS("totalcashandshortterminvestments.lasttwelvemonths", false, "Total Cash And Short Term Investments"),
    TOTALCOMMONEQUITY("totalcommonequity.lasttwelvemonths", false, "Total Common Equity"),
    TOTALCOMMONSHARESOUTSTANDING("totalcommonsharesoutstanding.lasttwelvemonths", false, "Total Common Shares Outstanding"),
    TOTALCURRENTASSETS("totalcurrentassets.lasttwelvemonths", false, "Total Current Assets"),
    TOTALCURRENTLIABILITIES("totalcurrentliabilities.lasttwelvemonths", false, "Total Current Liabilities"),
    TOTALDEBT("totaldebt.lasttwelvemonths", false, "Total Debt"),
    TOTALDEBTEBITDA("totaldebtebitda.lasttwelvemonths", false, "Total Debt / EBITDA"),
    TOTALDEBTEQUITY("totaldebtequity.lasttwelvemonths", false, "Total Debt/Equity"),
    TOTALEQUITY("totalequity.lasttwelvemonths", false, "Total Equity"),
    TOTALREVENUES("totalrevenues.lasttwelvemonths", false, "Total Revenues"),
    TOTALREVENUES1YRGROWTH("totalrevenues1yrgrowth.lasttwelvemonths", false, "Total Revenues, 1 Yr. Growth %"),
    TOTALSHARESOUTSTANDING("totalsharesoutstanding", false, "Total Shares Outstanding"),
    UNLEVEREDFREECASHFLOW("unleveredfreecashflow.lasttwelvemonths", false, "Unlevered Free Cash Flow");


    private final String value;
    private final boolean sortable;
    private final String displayString;

    ScreenerField(String value, boolean sortable, String displayString) {
        this.value = value;
        this.sortable = sortable;
        this.displayString = displayString;
    }

    public String getValue() {
        return value;
    }

    public boolean isSortable()
    {
        return sortable;
    }

    public String getDisplayString()
    {
        return displayString;
    }
}
