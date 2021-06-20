/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterFactory;
import com.github.bradjacobs.yahoofinance.response.YahooResponseConverter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * YahooClient helper class to for request to return a "list of maps" or "map of maps"
 *
 */
class CollectionRequestExecutor
{
    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH = 100L;


    // TODO - refactor to eliminate some of the duplication of the 2 methods.



    public List<Map<String,Object>> fetchListData(YahooFinanceClient client, YahooFinanceRequest request) throws IOException
    {
        YahooResponseConverter responseConverter = ResponseConverterFactory.getResponseConverter(request.getEndpoint());

        BatchableRequestStrategy batchableRequestStrategy = request.getBatchableRequestStrategy();
        if (batchableRequestStrategy == null) {
            // normal single request
            String responseJson = client.executeRequest(request);
            return responseConverter.convertToListOfMaps(responseJson);
        }
        else
        {
            // keep exeucting requests updating offset until we get everything.
            List<Map<String, Object>> totalResultList = new ArrayList<>();

            int batchSize = batchableRequestStrategy.getBatchSize();
            List<Map<String, Object>> responseObjectBatch;
            do {
                YahooFinanceRequest batchRequest = batchableRequestStrategy.buildNewRequest();

                String responseJson = client.executeRequest(batchRequest);
                responseObjectBatch = responseConverter.convertToListOfMaps(responseJson);
                totalResultList.addAll(responseObjectBatch);

                batchableRequestStrategy.incrementBatchOffset();
                batchIterationSleep();

            } while (responseObjectBatch.size() == batchSize);

            return totalResultList;
        }
    }

    public Map<String,Map<String,Object>> fetchMapData(YahooFinanceClient client, YahooFinanceRequest request) throws IOException
    {
        YahooResponseConverter responseConverter = ResponseConverterFactory.getResponseConverter(request.getEndpoint());

        BatchableRequestStrategy batchableRequestStrategy = request.getBatchableRequestStrategy();
        if (batchableRequestStrategy == null) {
            // normal single request
            String responseJson = client.executeRequest(request);
            return responseConverter.convertToMapOfMaps(responseJson);
        }
        else
        {
            // keep exeucting requests updating offset until we get everything.
            Map<String,Map<String,Object>> totalResultMap = new LinkedHashMap<>();

            int batchSize = batchableRequestStrategy.getBatchSize();
            Map<String,Map<String,Object>> responseObjectBatch;
            do {
                YahooFinanceRequest batchRequest = batchableRequestStrategy.buildNewRequest();

                String responseJson = client.executeRequest(batchRequest);
                responseObjectBatch = responseConverter.convertToMapOfMaps(responseJson);
                totalResultMap.putAll(responseObjectBatch);

                batchableRequestStrategy.incrementBatchOffset();
                batchIterationSleep();

            } while (responseObjectBatch.size() == batchSize);

            return totalResultMap;
        }
    }

    private void batchIterationSleep() {
        try {
            Thread.sleep(SLEEP_TIME_BETWEEN_BATCH);
        }
        catch (InterruptedException e) {
           /* ignore */
        }
    }

}
