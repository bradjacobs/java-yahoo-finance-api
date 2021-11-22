/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public class YahooFinanceBatchRequest implements YahooRequest
{
    private final YahooRequest innerRequest;
    private final BatchableRequestBuilder batchableRequestBuilder;

    public YahooFinanceBatchRequest(YahooRequest innerRequest, BatchableRequestBuilder batchableRequestBuilder) {
        this.innerRequest = innerRequest;
        this.batchableRequestBuilder = batchableRequestBuilder;
    }

    public BatchableRequestBuilder getBatchableRequestBuilder()
    {
        return batchableRequestBuilder;
    }


    @Override
    public YahooEndpoint getEndpoint() {
        return innerRequest.getEndpoint();
    }

    @Override
    public boolean isPost() {
        return innerRequest.isPost();
    }

    @Override
    public String getPostBody() {
        return innerRequest.getPostBody();
    }

    @Override
    public boolean isCrumbRequired() {
        return innerRequest.isCrumbRequired();
    }

    @Override
    public String getTicker() {
        return innerRequest.getTicker();
    }

    @Override
    public Map<String, String> getParamMap() {
        return innerRequest.getParamMap();
    }

    @Override
    public Map<String, String> getHeaderMap() {
        return innerRequest.getHeaderMap();
    }

}
