package com.github.bradjacobs.yahoofinance.response.batch;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class BatchResponseCheckerFactory
{
    // no reason to keep recreating these instances (have no state)
    private static final CountTotalPrefixBatchResponseChecker COUNT_TOTAL_PREFIX_CHECKER = new CountTotalPrefixBatchResponseChecker();
    private static final VisualizationResponseChecker VISUALIZATION_RESPONSE_CHECKER = new VisualizationResponseChecker();

    public BatchResponseChecker getBatchResponseChecker(YahooEndpoint endpoint)
    {
        if (endpoint == null) {
            return null;
        }
        switch (endpoint)
        {
            case SCREENER:
            case PREMIUM_SCREENER:
                return COUNT_TOTAL_PREFIX_CHECKER;
            case LOOKUP:
                return COUNT_TOTAL_PREFIX_CHECKER;
            case VISUALIZATION:
                return VISUALIZATION_RESPONSE_CHECKER;
            default:
                return null;
        }
    }
}
