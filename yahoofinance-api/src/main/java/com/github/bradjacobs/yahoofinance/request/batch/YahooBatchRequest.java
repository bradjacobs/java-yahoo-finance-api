/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

// TODO - ONE GIANT EXPEIMENT PLAYGROUND!

// TODO -- obviously needs javadocs

public class YahooBatchRequest implements YahooRequest, YahooBatchableRequest
{
    private final YahooRequest innerRequest;
    private final ParamMapBatchUpdater paramMapBatchUpdater;
    private final PostBodyBatchUpdater postBodyBatchUpdater;
    private final int batchSize;
    private final int maxResults;

    // todo - fix constructor param count craziness
    public YahooBatchRequest(YahooRequest innerRequest,
                             ParamMapBatchUpdater paramMapBatchUpdater,
                             PostBodyBatchUpdater postBodyBatchUpdater,
                             int batchSize,
                             int maxResults) {
        this.innerRequest = innerRequest;
        this.paramMapBatchUpdater = paramMapBatchUpdater;
        this.postBodyBatchUpdater = postBodyBatchUpdater;
        this.batchSize = batchSize;
        this.maxResults = maxResults;
    }

    @Override
    public int getBatchSize() {
        return batchSize;
    }

    @Override
    public int getMaxResults() {
        return maxResults;
    }

    public int getMaxBatchNumber() {
        int maxBatchNumber = maxResults / batchSize;
        if (maxResults % batchSize != 0) {
            maxBatchNumber++;
        }
        return maxBatchNumber;
    }


    @Override
    public Map<String, String> getParamMap(int batchNumber) {
        Map<String, String> paramMap = innerRequest.getParamMap();
        if (paramMapBatchUpdater != null) {
            paramMap = paramMapBatchUpdater.convert(paramMap, batchNumber);
        }
        return paramMap;
    }

    @Override
    public String getPostBody(int batchNumber) {
        String postBody = innerRequest.getPostBody();
        if (postBodyBatchUpdater != null) {
            postBody = postBodyBatchUpdater.convert(postBody, batchNumber);
        }
        return postBody;
    }


    @Override
    public YahooEndpoint getEndpoint() {
        return innerRequest.getEndpoint();
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
