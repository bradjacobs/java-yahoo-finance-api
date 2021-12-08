/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-12-07")
public enum StockValueDescription implements CriteriaEnum
{
    NEAR_FAIR_VALUE("Near Fair Value"),
    OVERVALUED("Over Valued"),
    UNDERVALUED("Under Valued");



    private final String value;

    StockValueDescription(String value) {
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
