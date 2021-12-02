package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

// todo - think of better names for classes
public class BatchResponseCheckerFactory
{
    public BatchResponseTerminationChecker getBatchResponseChecker(YahooEndpoint endpoint, int batchSize)
    {
        if (endpoint == null) {
            return null;
        }
        switch (endpoint)
        {
            case SCREENER:
            case PREMIUM_SCREENER:
            case LOOKUP:
                return new CountTotalPrefixBatchResponseChecker(batchSize);
            case VISUALIZATION:
                return new VisualizationResponseChecker(batchSize);
            default:
                return null;
        }
    }
}
