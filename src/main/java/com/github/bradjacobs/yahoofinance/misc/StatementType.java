/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.misc;

// NOTE: names and structure of this is very likely to change.
enum StatementType {
    INC_STMT("income_statement", true),
    BAL_SHEET("balance_sheet", false),
    CASH_FLOW("cash_flow", true),
    VALUE("valuation", true);

    private final String key;
    private final boolean allowsTtm;
    private StatementType(String key, boolean allowsTtm)
    {
        this.key = key;
        this.allowsTtm = allowsTtm;
    }
    public String getKey() { return key; }

    public boolean isTtmAllowed() {
        return allowsTtm;
    }
}
