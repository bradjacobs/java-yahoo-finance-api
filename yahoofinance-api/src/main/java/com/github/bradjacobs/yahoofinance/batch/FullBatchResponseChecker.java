package com.github.bradjacobs.yahoofinance.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

public interface FullBatchResponseChecker
{
    boolean isFullBatchResponse(Response response, int batchSize);
}
