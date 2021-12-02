/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaKey;
import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-12-01")
public enum EarningsEventField implements CriteriaKey
{
    DATEISESTIMATE("dateisestimate", false, "Estimate Date", false),
    ENDDATETIME("enddatetime", true, "Event End Date", false),
    EPSACTUAL("epsactual", true, "Reported EPS", false),
    EPSCONSENSUS("epsconsensus", true, "EPS Consensus", false),
    EPSESTIMATE("epsestimate", true, "EPS Estimate", false),
    EPSSURPRISEPCT("epssurprisepct", true, "Surprise (%)", false),
    EVENTID("eventid", false, "Event Id", false),
    EVENTNAME("eventname", false, "Event Name", false),
    EVENTTYPE("eventtype", false, "Event Type", false),
    FISCALYEAR("fiscalyear", false, "Financial Calendar Year", false),
    QUARTER("quarter", false, "Quarter", false),
    STARTDATETIME("startdatetime", true, "Event Start Date", false),
    STARTDATETIMETYPE("startdatetimetype", false, "Event Start Time", false);



    private final String value;
    private final boolean sortable;
    private final String displayString;
    private final boolean isPremium;

    EarningsEventField(String value, boolean sortable, String displayString, boolean isPremium) {
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
