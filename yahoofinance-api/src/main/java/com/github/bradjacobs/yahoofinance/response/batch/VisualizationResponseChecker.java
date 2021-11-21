package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.http.Response;

public class VisualizationResponseChecker implements BatchResponseChecker
{
    private static final String EMPTY_ROWS_IDENTIFIER = "\"rows\":[]";

    @Override
    public boolean isFullBatchResponse(Response response, int batchSize)
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
