package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

public interface BatchResponseTerminationChecker
{
    /**
     * Checks to see if a response is a "full batch response"
     *   i.e. return true if response contains number of results as the batchSize
     * @param response response
     * @return return TRUE if response contains 'full batch' number of entries within the response body.
     */
    boolean isFullBatchResponse(Response response);
}
