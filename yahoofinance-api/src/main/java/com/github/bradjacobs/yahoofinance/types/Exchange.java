/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.screener.CriteriaEnum;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-07-09")
public enum Exchange implements CriteriaEnum
{
    BSE("BSE"),
    NASDAQ("NAS"),
    NASDAQCM("NCM"),
    NASDAQGM("NGM"),
    NASDAQGS("NMS"),
    NYSE("NYQ"),
    PNK("PNK"),
    YHD("YHD");



    private final String value;

    Exchange(String value) {
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
