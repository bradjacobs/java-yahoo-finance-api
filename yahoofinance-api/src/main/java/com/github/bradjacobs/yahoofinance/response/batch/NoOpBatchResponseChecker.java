package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

// todo - this class is probably temporary and should eventually be removed.
public class NoOpBatchResponseChecker implements BatchResponseTerminationChecker
{
    public NoOpBatchResponseChecker() {
    }

    @Override
    public boolean isFullBatchResponse(Response response)
    {
        if (response == null || response.isError()) {
            return false;
        }

        return true;
    }
}
