/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-06-19")
public enum Interval
{
    ONE_MIN("1m"),
    TWO_MIN("2m"),
    FIVE_MIN("5m"),
    FIFTEEN_MIN("15m"),
    THIRTY_MIN("30m"),
    SIXTY_MIN("60m"),
    NINETY_MIN("90m"),
    ONE_HOUR("1h"),
    ONE_DAY("1d"),
    FIVE_DAYS("5d"),
    ONE_WEEK("1wk"),
    ONE_MONTH("1mo"),
    THREE_MONTHS("3mo");


    private final String value;

    Interval(String value) {
        this.value = value;

    }

    public String getValue() {
        return value;
    }
}
