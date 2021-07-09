package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class BatchableRequestExecutor
{
    private final YahooFinanceClient client;

    private static final String COUNT_PREFIX = "\"count\":";
    private static final String TOTAL_PREFIX = "\"total\":";
    private static final int RESPONSE_INTRO_SIZE = 300; // how much of the first part of the response to analyze.

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;


    public BatchableRequestExecutor(YahooFinanceClient client)
    {
        this.client = client;
    }

    public List<Response> executeRequest(YahooFinanceBatchRequest batachableRequest) throws IOException
    {
        List<Response> responseList = new ArrayList<>();

        BatchableRequestStrategy batchableRequestStrategy = batachableRequest.getBatchableRequestStrategy();
        int batchSize = batchableRequestStrategy.getBatchSize();
        int originalBatchOffset = batchableRequestStrategy.getBatchOffset();
        int currentBatchOffset = originalBatchOffset;

        Response response = null;
        boolean continueBatchRequesting = true;

        try
        {
            do {
                YahooFinanceRequest batchRequest = batchableRequestStrategy.buildNewRequest();
                response = client.executeInternal(batchRequest);
                responseList.add(response);

                continueBatchRequesting = shouldContinueBatchRequesting(batchSize, response);
                if (continueBatchRequesting)
                {
                    currentBatchOffset = currentBatchOffset + batchSize;
                    batchableRequestStrategy.setBatchOffset(currentBatchOffset);
                    batchIterationSleep();
                }
            } while (continueBatchRequesting);

        }
        finally
        {
            batchableRequestStrategy.setBatchOffset(originalBatchOffset);
        }

        return responseList;
    }


    private static void batchIterationSleep() {
        try { Thread.sleep(SLEEP_TIME_BETWEEN_BATCH_REQUESTS); }
        catch (InterruptedException e) {/* ignore */ }
    }




    private boolean shouldContinueBatchRequesting(int batchSize, Response response)
    {
        if (response == null || response.isError()) {
            return false;
        }

        // Note: not interested in parsing out HUGE response body, thus just grab the 'first part' of the response
        //   to determine the information required.
        String responseBody = response.getBody();
        String responseBodySubstring = responseBody.substring(0, Math.min(responseBody.length(), RESPONSE_INTRO_SIZE));

        int count = 0;
        int total = 0;

        try {
            count = Integer.valueOf( StringUtils.substringBetween(responseBodySubstring, COUNT_PREFIX, ",") );
            total = Integer.valueOf( StringUtils.substringBetween(responseBodySubstring, TOTAL_PREFIX, ",") );
        }
        catch (Exception e) {
            /* ignore (for now) */
        }

        if (count == 0 || total == 0 || (count != batchSize) || (count == total)) {
            return false;
        }
        return true;
    }

}