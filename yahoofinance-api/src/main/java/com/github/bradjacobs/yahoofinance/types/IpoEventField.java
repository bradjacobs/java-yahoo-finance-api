/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaKey;
import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-12-01")
public enum IpoEventField implements CriteriaKey
{
    AMENDEDDATE("amendeddate", true, "Amended Date", false),
    CURRENCYNAME("currencyname", false, "Currency", false),
    DEALNO("dealno", false, "Deal ID", false),
    DEALTYPE("dealtype", false, "Action", false),
    EXCHANGE("exchange", false, "Exchange", false),
    EXCHANGE_SHORT_NAME("exchange_short_name", false, "Exchange Short Name", false),
    FILINGDATE("filingdate", true, "Filing Date", false),
    INVESTORTYPE("investortype", false, "Investor Type", false),
    OFFERPRICE("offerprice", true, "Price", false),
    PRICEFROM("pricefrom", true, "Price From", false),
    PRICETO("priceto", true, "Price To", false),
    QUOTETYPE("quotetype", false, "Quote Type", false),
    SHARES("shares", true, "Shares", false),
    STARTDATETIME("startdatetime", true, "Date", false),
    TICKER("ticker", true, "Symbol", false);



    private final String value;
    private final boolean sortable;
    private final String displayString;
    private final boolean isPremium;

    IpoEventField(String value, boolean sortable, String displayString, boolean isPremium) {
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
