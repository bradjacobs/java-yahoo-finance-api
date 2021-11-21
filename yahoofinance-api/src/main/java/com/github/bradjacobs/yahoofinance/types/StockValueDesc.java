/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;

/**
 * Enum used with Stock Screener endpoint:
 *  *****  Only applicable for 'Premium' *****
 */
//  note: seemed overkill to auto-gen this
public enum StockValueDesc implements CriteriaEnum
{
    UNDERVALUED("Under Valued"),
    FAIRVALUE("Near Fair Value"),
    OVERVALUED("Over Valued");


    private final String value;

    StockValueDesc(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    @Override
    public String getCriteriaValue() {
        return value;
    }
}
