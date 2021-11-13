/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public class YahooFinanceBatchRequest extends YahooFinanceRequest
{
    private final BatchableRequestBuilder batchableRequestBuilder;

    public YahooFinanceBatchRequest(YahooFinanceRequest request,
                                    BatchableRequestBuilder batchableRequestBuilder)
    {
        this(request.endpoint, request.ticker, request.paramMap, request.postBody, request.headerMap, batchableRequestBuilder);
    }


    public YahooFinanceBatchRequest(YahooEndpoint endpoint, String ticker,
                                    Map<String, String> paramMap, Object postBody, Map<String,String> headerMap,
        BatchableRequestBuilder batchableRequestBuilder)
    {
        super(endpoint, ticker, paramMap, postBody, headerMap);
        this.batchableRequestBuilder = batchableRequestBuilder;
    }

    public BatchableRequestBuilder getBatchableRequestBuilder()
    {
        return batchableRequestBuilder;
    }

}
