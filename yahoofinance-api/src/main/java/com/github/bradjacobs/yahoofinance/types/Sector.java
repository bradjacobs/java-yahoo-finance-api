/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;

import javax.annotation.Generated;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-06-19")
public enum Sector implements CriteriaEnum
{
    BASIC_MATERIALS("Basic Materials"),
    COMMUNICATION_SERVICES("Communication Services"),
    CONSUMER_CYCLICAL("Consumer Cyclical"),
    CONSUMER_DEFENSIVE("Consumer Defensive"),
    ENERGY("Energy"),
    FINANCIAL_SERVICES("Financial Services"),
    HEALTHCARE("Healthcare"),
    INDUSTRIALS("Industrials"),
    REAL_ESTATE("Real Estate"),
    TECHNOLOGY("Technology"),
    UTILITIES("Utilities");


    private final String value;

    Sector(String value) {
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
