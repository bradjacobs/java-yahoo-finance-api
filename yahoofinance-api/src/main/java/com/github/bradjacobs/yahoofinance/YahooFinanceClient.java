/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.http.exception.HttpExceptionFactory;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.RequestUrlGenerator;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.response.YahooCompositeResponse;
import com.github.bradjacobs.yahoofinance.response.YahooSimpleResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseTerminationChecker;
import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseCheckerFactory;
import org.apache.http.HttpHeaders;
import org.apache.http.entity.ContentType;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class YahooFinanceClient
{
    private static final String DEFAULT_CONTENT_TYPE = ContentType.APPLICATION_JSON.getMimeType();

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;
    private final RequestUrlGenerator requestUrlGenerator;
    private final BatchResponseCheckerFactory batchResponseCheckerFactory = new BatchResponseCheckerFactory();

    // todo: for the moment this is always true.. to fix.
    private final boolean throwExceptionOnHttpError = true;

    public YahooFinanceClient()
    {
        this(HttpClientAdapterFactory.createDefaultClient());
    }

    public YahooFinanceClient(HttpClientAdapter httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;

        // important note:  the CrumbDataSource _MUST_ use the SAME
        //     httpClient that is used by the YahooClient!
        this.crumbDataSource = new CrumbDataSource(httpClient);
        this.requestUrlGenerator = new RequestUrlGenerator(crumbDataSource);
    }

    public YahooResponse execute(YahooRequest request) throws IOException
    {
        // todo - come back to address this (a little kludgy)
        if (request instanceof YahooBatchRequest) {
            return executeBatch((YahooBatchRequest)request);
        }

        Response rawResponse = executeInternal(request);
        return new YahooSimpleResponse(request.getEndpoint(), rawResponse);
    }


    protected Response executeInternal(YahooRequest request) throws IOException
    {
        String url = requestUrlGenerator.buildRequestUrl(request);
        String postBody = request.getPostBody();
        Map<String,String> headerMap = createAdditionalRequestHeaderMap(request);
        return executeInternal(url, postBody, headerMap);
    }

    protected Response executeInternal(String url, String postBody, Map<String,String> headerMap) throws IOException
    {
        Response response;
        if (postBody != null) {
            response = httpClient.executePost(url, postBody, headerMap);
        }
        else {
            response = httpClient.executeGet(url, headerMap);
        }

        if (response.isError() && this.throwExceptionOnHttpError) {
            throw HttpExceptionFactory.createException(response);
        }
        return response;
    }

    /**
     * Map of additional headers to be added to the request.
     *   It's assumed the underlying httpClient usually has a few common
     *   http request headers already set (e.g. 'accept-encoding')
     */
    private Map<String,String> createAdditionalRequestHeaderMap(YahooRequest request)
    {
        Map<String,String> headerMap = new LinkedHashMap<>(request.getHeaderMap());
        if (!headerMap.containsKey(HttpHeaders.CONTENT_TYPE)) {
            headerMap.put(HttpHeaders.CONTENT_TYPE, DEFAULT_CONTENT_TYPE);
        }
        return headerMap;
    }

    public YahooResponse executeBatch(YahooBatchRequest request) throws IOException
    {
        BatchResponseTerminationChecker batchResponseChecker = batchResponseCheckerFactory.getBatchResponseChecker(request);
        Map<String,String> headerMap = createAdditionalRequestHeaderMap(request);

        List<Response> responseList = new ArrayList<>();
        boolean continueBatchRequesting;

        int batchNumber = 1;
        int maxBatchNumber = request.getMaxBatchNumber();

        do {
            Map<String, String> paramMap = request.getParamMap(batchNumber);
            String postBody = request.getPostBody(batchNumber);
            String url = requestUrlGenerator.buildRequestUrl(request, paramMap);

            Response response = executeInternal(url, postBody, headerMap);
            responseList.add(response);

            if (batchNumber >= maxBatchNumber) {
                break;
            }

            continueBatchRequesting = batchResponseChecker.isFullBatchResponse(response);
            if (continueBatchRequesting) {
                try { Thread.sleep(SLEEP_TIME_BETWEEN_BATCH_REQUESTS); }
                catch (InterruptedException e) {/* ignore exception */ }
            }
            batchNumber++;
        } while (continueBatchRequesting);

        return new YahooCompositeResponse(request.getEndpoint(), responseList, request.getMaxResults());
    }
}
