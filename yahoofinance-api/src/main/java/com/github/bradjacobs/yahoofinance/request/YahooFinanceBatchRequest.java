/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;


import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public class YahooFinanceBatchRequest extends YahooFinanceRequest
{
    private final BatchableRequestStrategy batchableRequestStrategy;

    public YahooFinanceBatchRequest(YahooEndpoint endpoint, String ticker, Map<String, String> paramMap, Object postBody,
        BatchableRequestStrategy batchableRequestStrategy)
    {
        super(endpoint, ticker, paramMap, postBody);
        this.batchableRequestStrategy = batchableRequestStrategy;
    }

    public BatchableRequestStrategy getBatchableRequestStrategy()
    {
        return batchableRequestStrategy;
    }
}
