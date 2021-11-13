package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.batch.FullBatchResponseChecker;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class BatchableRequestExecutor
{
    private final YahooFinanceClient client;

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;


    public BatchableRequestExecutor(YahooFinanceClient client)
    {
        this.client = client;
    }

    public List<Response> executeRequest(YahooFinanceBatchRequest batachableRequest) throws IOException
    {
        BatchableRequestStrategy batchableRequestStrategy = batachableRequest.getBatchableRequestStrategy();

        if (batchableRequestStrategy == null) {
            return Collections.singletonList(client.executeInternal(batachableRequest));
        }

        // todo: fix -- shouldn't get this from the request.
        FullBatchResponseChecker fullBatchResponseChecker = batachableRequest.getFullBatchResponseChecker();

        List<Response> responseList = new ArrayList<>();

        int batchSize = batchableRequestStrategy.getBatchSize();
        int originalBatchOffset = batchableRequestStrategy.getBatchOffset();
        int currentBatchOffset = originalBatchOffset;

        Response response;
        boolean continueBatchRequesting = true;

        try
        {
            do {
                YahooFinanceRequest batchRequest = batchableRequestStrategy.buildNewRequest();
                response = client.executeInternal(batchRequest);
                responseList.add(response);

                continueBatchRequesting = fullBatchResponseChecker.isFullBatchResponse(response, batchSize);
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

}
