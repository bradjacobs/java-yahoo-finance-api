package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

public interface BatchResponseChecker
{
    /**
     * Checks to see if a response if a "full batch response"
     *   i.e. return true if response contains number of results as the batchSize
     * @param response response
     * @param batchSize batchSize
     * @return return TRUE if response contains 'batchSize' number of entries within the response body.
     */
    boolean isFullBatchResponse(Response response, int batchSize);
}
