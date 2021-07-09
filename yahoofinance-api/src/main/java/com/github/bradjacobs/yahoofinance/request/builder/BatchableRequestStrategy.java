/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;

public interface BatchableRequestStrategy
{
    int getBatchSize();
    int getBatchOffset();
    void setBatchOffset(int offset);

    YahooFinanceRequest buildNewRequest();
}
