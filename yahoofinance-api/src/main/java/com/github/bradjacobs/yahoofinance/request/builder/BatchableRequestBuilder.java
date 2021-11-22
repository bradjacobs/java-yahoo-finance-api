/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public interface BatchableRequestBuilder
{
    YahooEndpoint getEndpoint();
    int getBatchSize();
    int getBatchOffset();
    void setBatchOffset(int offset);

    YahooRequest build();
}
