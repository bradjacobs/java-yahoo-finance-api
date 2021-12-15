package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

// todo - think of better names for classes
public class BatchResponseCheckerFactory
{
    public BatchResponseTerminationChecker getBatchResponseChecker(YahooBatchRequest request)
    {
        YahooEndpoint endpoint = request.getEndpoint();
        int batchSize = request.getBatchSize();

        switch (endpoint)
        {
            case SCREENER:
            case PREMIUM_SCREENER:
            case LOOKUP:
                return new CountTotalPrefixBatchResponseChecker(batchSize);
            case VISUALIZATION:
                return new VisualizationResponseChecker(batchSize);
            case QUOTE:
                return new NoOpBatchResponseChecker();
            default:
                throw new IllegalStateException("No BatchResponseChecker found for endpoint: " + request.getEndpoint());
        }
    }
}
