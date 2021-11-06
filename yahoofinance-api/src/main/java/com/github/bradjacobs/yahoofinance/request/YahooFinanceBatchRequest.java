/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.batch.FullBatchResponseChecker;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public class YahooFinanceBatchRequest extends YahooFinanceRequest
{
    private final BatchableRequestStrategy batchableRequestStrategy;
    private final FullBatchResponseChecker fullBatchResponseChecker;

    // TODO - fix.... this really isn't the correct spot to put the fullBatchResposneChecker.

    public YahooFinanceBatchRequest(YahooEndpoint endpoint, String ticker,
                                    Map<String, String> paramMap, Object postBody, Map<String,String> headerMap,
        BatchableRequestStrategy batchableRequestStrategy,
        FullBatchResponseChecker fullBatchResponseChecker)
    {
        super(endpoint, ticker, paramMap, postBody, headerMap);
        this.batchableRequestStrategy = batchableRequestStrategy;
        this.fullBatchResponseChecker = fullBatchResponseChecker;
    }

    public BatchableRequestStrategy getBatchableRequestStrategy()
    {
        return batchableRequestStrategy;
    }

    public FullBatchResponseChecker getFullBatchResponseChecker() {
        return fullBatchResponseChecker;
    }
}
