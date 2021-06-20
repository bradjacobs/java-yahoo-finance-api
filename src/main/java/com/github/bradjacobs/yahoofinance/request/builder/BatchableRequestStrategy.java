/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

public interface BatchableRequestStrategy
{
    int getBatchSize();
    int getCurrentOffset();
    void incrementBatchOffset();

    YahooFinanceRequest buildNewRequest();
}
