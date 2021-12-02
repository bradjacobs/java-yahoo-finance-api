package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

public class VisualizationResponseChecker implements BatchResponseTerminationChecker
{
    private static final String EMPTY_ROWS_IDENTIFIER = "\"rows\":[]";

    private final int batchSize;

    public VisualizationResponseChecker(int batchSize) {
        this.batchSize = batchSize;
    }

    @Override
    public boolean isFullBatchResponse(Response response)
    {
        if (response == null || response.isError()) {
            return false;
        }

        String responseBody = response.getBody();
        if (responseBody.contains(EMPTY_ROWS_IDENTIFIER)) {
            return false;
        }
        return true;
    }
}
