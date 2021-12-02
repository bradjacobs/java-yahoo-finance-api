/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaKey;
import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-12-01")
public enum ScreenerField implements CriteriaKey
{
    // Basic Fields
    ALTMANZSCOREUSINGTHEAVERAGESTOCKINFORMATIONFORAPERIOD("altmanzscoreusingtheaveragestockinformationforaperiod.lasttwelvemonths", true, "Altman Z Score Using the Average Stock Information for a Period", false),
    AVERAGE_ANALYST_RATING("average_analyst_rating", true, "Avg. Analyst Rating", false),
    AVGDAILYVOL3M("avgdailyvol3m", true, "Average Daily 3m Volume", false),
    BASICEPSCONTINUINGOPERATIONS("basicepscontinuingoperations.lasttwelvemonths", true, "Basic EPS - Continuing Operations", false),
    BEARISH_CONFIDENCE_LOWER_BOUND("bearish_confidence_lower_bound", true, "Bearish Confidence Lower Bound", false),
    BEARISH_COUNT("bearish_count", true, "Bearish Count", false),
    BEARISH_PROPORTION("bearish_proportion", true, "Bearish Proportion", false),
    BETA("beta", true, "Beta", false),
    BOOKVALUESHARE("bookvalueshare.lasttwelvemonths", true, "Book Value / Share", false),
    BOUGHT_CONFIDENCE_LOWER_BOUND("bought_confidence_lower_bound", true, "Bought Confidence Lower Bound", false),
    BOUGHT_COUNT("bought_count", true, "Bought Count", false),
    BOUGHT_PROPORTION("bought_proportion", true, "Bought Proportion", false),
    BULLISH_CONFIDENCE_LOWER_BOUND("bullish_confidence_lower_bound", true, "Bullish Confidence Lower Bound", false),
    BULLISH_COUNT("bullish_count", true, "Bullish Count", false),
    BULLISH_PROPORTION("bullish_proportion", true, "Bullish Proportion", false),
    CAPITALEXPENDITURE("capitalexpenditure.lasttwelvemonths", true, "Capital Expenditure", false),
    CASHFROMOPERATIONS("cashfromoperations.lasttwelvemonths", true, "Cash from Operations", false),
    CASHFROMOPERATIONS1YRGROWTH("cashfromoperations1yrgrowth.lasttwelvemonths", true, "Cash From Operations, 1 Yr. Growth %", false),
    CONSECUTIVE_YEARS_OF_DIVIDEND_GROWTH_COUNT("consecutive_years_of_dividend_growth_count", true, "Consecutive Years of Dividend Growth Count", false),
    CURRENTRATIO("currentratio.lasttwelvemonths", true, "Current Ratio", false),
    DAYS_TO_COVER_SHORT("days_to_cover_short.value", true, "Short Interest Ratio", false),
    DAYVOLUME("dayvolume", true, "Day Volume", false),
    DILUTEDEPS1YRGROWTH("dilutedeps1yrgrowth.lasttwelvemonths", true, "Diluted EPS, 1 Yr. Growth %", false),
    DILUTEDEPSCONTINUINGOPERATIONS("dilutedepscontinuingoperations.lasttwelvemonths", true, "Diluted EPS - Continuing Operations", false),
    EBIT("ebit.lasttwelvemonths", true, "EBIT", false),
    EBITDA("ebitda.lasttwelvemonths", true, "EBITDA", false),
    EBITDA1YRGROWTH("ebitda1yrgrowth.lasttwelvemonths", true, "EBITDA, 1 Yr. Growth %", false),
    EBITDAINTERESTEXPENSE("ebitdainterestexpense.lasttwelvemonths", true, "EBITDA / Interest Expense", false),
    EBITDAMARGIN("ebitdamargin.lasttwelvemonths", true, "EBITDA Margin %", false),
    EBITINTERESTEXPENSE("ebitinterestexpense.lasttwelvemonths", true, "EBIT / Interest Expense", false),
    ENVIRONMENTAL_SCORE("environmental_score", true, "Environmental Score", false),
    EODPRICE("eodprice", true, "EOD Price", false),
    EODVOLUME("eodvolume", true, "EOD Volume", false),
    EPSGROWTH("epsgrowth.lasttwelvemonths", true, "EPS Growth", false),
    ESG_SCORE("esg_score", true, "ESG Score", false),
    EXCHANGE("exchange", false, "Exchange", false),
    FIFTYTWOWKPERCENTCHANGE("fiftytwowkpercentchange", true, "52 Week Percent Change", false),
    FORWARD_DIVIDEND_PER_SHARE("forward_dividend_per_share", true, "Forward Dividend Per Share", false),
    FORWARD_DIVIDEND_YIELD("forward_dividend_yield", true, "Forward Dividend Yield", false),
    GOVERNANCE_SCORE("governance_score", true, "Governance Score", false),
    GROSSPROFIT("grossprofit.lasttwelvemonths", true, "Gross Profit", false),
    GROSSPROFITMARGIN("grossprofitmargin.lasttwelvemonths", true, "Gross Profit Margin %", false),
    HIGHEST_CONTROVERSY("highest_controversy", true, "Highest Controversy", false),
    INDUSTRY("industry", true, "Industry", false),
    INTRADAYMARKETCAP("intradaymarketcap", true, "Intraday Market Cap", false),
    INTRADAYPRICE("intradayprice", true, "Intraday Price", false),
    INTRADAYPRICECHANGE("intradaypricechange", true, "Change", false),
    LASTCLOSE52WEEKHIGH("lastclose52weekhigh.lasttwelvemonths", true, "Last Close 52 Week High", false),
    LASTCLOSE52WEEKLOW("lastclose52weeklow.lasttwelvemonths", true, "Last Close 52 Week Low", false),
    LASTCLOSEMARKETCAP("lastclosemarketcap.lasttwelvemonths", true, "Last Close Market Cap", false),
    LASTCLOSEMARKETCAPTOTALREVENUE("lastclosemarketcaptotalrevenue.lasttwelvemonths", true, "Last Close Market Cap / Total Revenue", false),
    LASTCLOSEPRICEBOOKVALUE("lastclosepricebookvalue.lasttwelvemonths", true, "Last Close Price / Book Value", false),
    LASTCLOSEPRICEEARNINGS("lastclosepriceearnings.lasttwelvemonths", true, "Last Close Price / Earnings", false),
    LASTCLOSEPRICETANGIBLEBOOKVALUE("lastclosepricetangiblebookvalue.lasttwelvemonths", true, "Last Close Price / Tangible Book Value", false),
    LASTCLOSETEVEBIT("lastclosetevebit.lasttwelvemonths", true, "Last Close TEV / EBIT", false),
    LASTCLOSETEVEBITDA("lastclosetevebitda.lasttwelvemonths", true, "Last Close TEV / EBITDA", false),
    LASTCLOSETEVTOTALREVENUE("lastclosetevtotalrevenue.lasttwelvemonths", true, "Last Close TEV / Total Revenue", false),
    LEVEREDFREECASHFLOW("leveredfreecashflow.lasttwelvemonths", true, "Levered Free Cash Flow", false),
    LEVEREDFREECASHFLOW1YRGROWTH("leveredfreecashflow1yrgrowth.lasttwelvemonths", true, "Levered Free Cash Flow, 1 Yr. Growth %", false),
    LTDEBTEQUITY("ltdebtequity.lasttwelvemonths", true, "LT Debt/Equity", false),
    NETDEBTEBITDA("netdebtebitda.lasttwelvemonths", true, "Net Debt / EBITDA", false),
    NETEPSBASIC("netepsbasic.lasttwelvemonths", true, "Net EPS - Basic", false),
    NETEPSDILUTED("netepsdiluted.lasttwelvemonths", true, "Net EPS - Diluted", false),
    NETINCOME1YRGROWTH("netincome1yrgrowth.lasttwelvemonths", true, "Net Income, 1 Yr. Growth %", false),
    NETINCOMEIS("netincomeis.lasttwelvemonths", true, "Net Income - (IS)", false),
    NETINCOMEMARGIN("netincomemargin.lasttwelvemonths", true, "Net Income Margin %", false),
    NEUTRAL_COUNT("neutral_count", true, "Neutral Count", false),
    NEUTRAL_PROPORTION("neutral_proportion", true, "Neutral Proportion", false),
    OPERATINGCASHFLOWTOCURRENTLIABILITIES("operatingcashflowtocurrentliabilities.lasttwelvemonths", true, "Operating Cash Flow to Current Liabilities", false),
    OPERATINGINCOME("operatingincome.lasttwelvemonths", true, "Operating Income", false),
    PCTHELDINSIDER("pctheldinsider", true, "Percent of shares held by insiders", false),
    PCTHELDINST("pctheldinst", true, "Percent of shares held by institutions", false),
    PEER_GROUP("peer_group", true, "Peer Group", false),
    PEGRATIO_5Y("pegratio_5y", true, "PEG Ratio (5 yr expected)", false),
    PERATIO("peratio.lasttwelvemonths", true, "Trailing P/E", false),
    PERCENTCHANGE("percentchange", true, "Percent Change", false),
    PRICEBOOKRATIO("pricebookratio.quarterly", true, "Price/Book", false),
    QUARTERLYREVENUEGROWTH("quarterlyrevenuegrowth.quarterly", true, "Quarterly Revenue Growth", false),
    QUICKRATIO("quickratio.lasttwelvemonths", true, "Quick Ratio", false),
    REGION("region", true, "Region", false),
    RETURNONASSETS("returnonassets.lasttwelvemonths", true, "Return on Assets", false),
    RETURNONEQUITY("returnonequity.lasttwelvemonths", true, "Return On Equity %", false),
    RETURNONTOTALCAPITAL("returnontotalcapital.lasttwelvemonths", true, "Return on Total Capital", false),
    SECTOR("sector", true, "Sector", false),
    SHORT_INTEREST("short_interest.value", true, "Short Interest", false),
    SHORT_INTEREST_PERCENTAGE_CHANGE("short_interest_percentage_change.value", true, "Short Interest % Change", false),
    SHORT_PERCENTAGE_OF_FLOAT("short_percentage_of_float.value", true, "Short % of Float", false),
    SHORT_PERCENTAGE_OF_SHARES_OUTSTANDING("short_percentage_of_shares_outstanding.value", true, "Short % of Shares Outstanding", false),
    SOCIAL_SCORE("social_score", true, "Social Score", false),
    SOLD_CONFIDENCE_LOWER_BOUND("sold_confidence_lower_bound", true, "Sold Confidence Lower Bound", false),
    SOLD_COUNT("sold_count", true, "Sold Count", false),
    SOLD_PROPORTION("sold_proportion", true, "Sold Proportion", false),
    TOTAL_COMMENTER_COUNT("total_commenter_count", true, "Total Commenter Count", false),
    TOTALASSETS("totalassets.lasttwelvemonths", true, "Total Assets", false),
    TOTALCASHANDSHORTTERMINVESTMENTS("totalcashandshortterminvestments.lasttwelvemonths", true, "Total Cash And Short Term Investments", false),
    TOTALCOMMONEQUITY("totalcommonequity.lasttwelvemonths", true, "Total Common Equity", false),
    TOTALCOMMONSHARESOUTSTANDING("totalcommonsharesoutstanding.lasttwelvemonths", true, "Total Common Shares Outstanding", false),
    TOTALCURRENTASSETS("totalcurrentassets.lasttwelvemonths", true, "Total Current Assets", false),
    TOTALCURRENTLIABILITIES("totalcurrentliabilities.lasttwelvemonths", true, "Total Current Liabilities", false),
    TOTALDEBT("totaldebt.lasttwelvemonths", true, "Total Debt", false),
    TOTALDEBTEBITDA("totaldebtebitda.lasttwelvemonths", true, "Total Debt / EBITDA", false),
    TOTALDEBTEQUITY("totaldebtequity.lasttwelvemonths", true, "Total Debt/Equity", false),
    TOTALEQUITY("totalequity.lasttwelvemonths", true, "Total Equity", false),
    TOTALREVENUES("totalrevenues.lasttwelvemonths", true, "Total Revenues", false),
    TOTALREVENUES1YRGROWTH("totalrevenues1yrgrowth.lasttwelvemonths", true, "Total Revenues, 1 Yr. Growth %", false),
    TOTALSHARESOUTSTANDING("totalsharesoutstanding", false, "Total Shares Outstanding", false),
    UNLEVEREDFREECASHFLOW("unleveredfreecashflow.lasttwelvemonths", true, "Unlevered Free Cash Flow", false),
    
    // Premium Fields
    CHANGE_IN_NUMBER_OF_INSTITUTIONAL_HOLDERS("change_in_number_of_institutional_holders", true, "Change in # of Institutional Holders", true),
    EARNINGS_CONSISTENCY("earnings_consistency", false, "Earnings Consistency", true),
    ESTIMATED_EARNINGS_GROWTH("estimated_earnings_growth", true, "Estimated Earnings Growth YoY %", true),
    ESTIMATED_REVENUE_GROWTH("estimated_revenue_growth", true, "Estimated Revenue Growth YoY %", true),
    HOLDERS_FUND_TYPES("holders_fund_types", false, "Institutional Owner Types", true),
    MORNINGSTAR_ECONOMIC_MOAT("morningstar_economic_moat", false, "Economic Moat", true),
    MORNINGSTAR_LAST_CLOSE_PRICE_TO_FAIR_VALUE("morningstar_last_close_price_to_fair_value", true, "Last Close Price To Morningstar Fair Value Ratio", true),
    MORNINGSTAR_MOAT_TREND("morningstar_moat_trend", false, "Moat Trend", true),
    MORNINGSTAR_RATING("morningstar_rating", true, "Morningstar Rating", true),
    MORNINGSTAR_RATING_CHANGE("morningstar_rating_change", false, "Morningstar Rating Change", true),
    MORNINGSTAR_RATING_UPDATED_TIME("morningstar_rating_updated_time", true, "Morningstar Ratings Updated Time", true),
    MORNINGSTAR_STEWARDSHIP("morningstar_stewardship", false, "Stewardship", true),
    MORNINGSTAR_UNCERTAINTY("morningstar_uncertainty", false, "Uncertainty Rating", true),
    NUMBER_OF_INSTITUTIONAL_BUYERS("number_of_institutional_buyers", true, "# of Institutional Buyers", true),
    NUMBER_OF_INSTITUTIONAL_HOLDERS("number_of_institutional_holders", true, "# of Institutional Holders", true),
    NUMBER_OF_INSTITUTIONAL_SELLERS("number_of_institutional_sellers", true, "# of Institutional Sellers", true),
    PERCENT_CHANGE_IN_NUMBER_OF_INSTITUTIONAL_HOLDERS("percent_change_in_number_of_institutional_holders", true, "% Change in # of Institutional Holders", true),
    PERCENT_CHANGE_IN_SHARES_HELD_BY_FUNDS("percent_change_in_shares_held_by_funds", true, "% Change in Shares Held by Institutions", true),
    PERCENT_IN_FUNDS_HOLDING("percent_in_funds_holding", true, "Institutional Holders %", true),
    PERCENT_IN_TOP_TEN_HOLDINGS("percent_in_top_ten_holdings", true, "% in Top 10 Holdings of Institutions", true),
    PERCENT_OF_SHARES_OUTSTANDING_BOUGHT_BY_INSTITUTIONS("percent_of_shares_outstanding_bought_by_institutions", true, "% of Shares Outstanding Bought by Institutions", true),
    PERCENT_OF_SHARES_OUTSTANDING_SOLD_BY_INSTITUTIONS("percent_of_shares_outstanding_sold_by_institutions", true, "% of Shares Outstanding Sold by Institutions", true),
    REVENUE_CONSISTENCY("revenue_consistency", false, "Revenue Consistency", true),
    ROR_PERCENT("ror_percent", true, "Rate of Return", true),
    TRADING_CENTRAL_LAST_CLOSE_PRICE_TO_FAIR_VALUE("trading_central_last_close_price_to_fair_value", true, "Last Close Price / Fair Value", true),
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

    @Override
    public String getKeyName() {
        return value;
    }
}
