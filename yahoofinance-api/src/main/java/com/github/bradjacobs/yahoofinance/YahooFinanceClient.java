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
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponseGenerator;
import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseChecker;
import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseCheckerFactory;
import com.github.bradjacobs.yahoofinance.response.batch.YahooBatchResponse;
import org.apache.http.HttpHeaders;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class YahooFinanceClient
{
    private static final Map<String,String> DEFAULT_HEADER_MAP = new LinkedHashMap<String,String>(){{
        put(HttpHeaders.CONTENT_TYPE, "application/json");
        put(HttpHeaders.USER_AGENT, "Java-Http-Client/11.0.0");  // TBD what a 'good' value should be
    }};

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;
    private final RequestUrlGenerator requestUrlGenerator = new RequestUrlGenerator();
    private final YahooResponseGenerator yahooResponseGenerator = new YahooResponseGenerator();
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
        this.crumbDataSource = new CrumbDataSource(httpClient);
    }

    public YahooResponse execute(YahooFinanceRequest request) throws IOException
    {
        Response rawResponse = executeInternal(request);
        return yahooResponseGenerator.makeRessponse(request, rawResponse);
    }

    public YahooBatchResponse executeBatch(YahooFinanceRequest request) throws IOException
    {
        // todo - come back to address this (a little kludgy)
        if (request instanceof YahooFinanceBatchRequest) {
            return executeBatch((YahooFinanceBatchRequest)request);
        }
        else {
            throw new IllegalArgumentException("Error: cannot execute batch request.  The request is not batchable.");
        }
    }

    public YahooBatchResponse executeBatch(YahooFinanceBatchRequest request) throws IOException
    {
        BatchableRequestBuilder batchRequestBuilder = request.getBatchableRequestBuilder();
        List<Response> rawResponses = executeBatchRequests(batchRequestBuilder);
        return yahooResponseGenerator.makeBatchResponse(request, rawResponses);
    }

    protected Response executeInternal(YahooFinanceRequest request) throws IOException
    {
        String crumb = null;
        if (request.isCrumbRequired()) {
            crumb = crumbDataSource.getCrumb(request);
        }

        String url = requestUrlGenerator.buildRequestUrl(request, crumb);
        Map<String,String> headerMap = createRequestHeaderMap(request);

        Response response = null;
        if (request.isPost()) {
            String postBody = request.getPostBody();
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

    private Map<String,String> createRequestHeaderMap(YahooFinanceRequest request)
    {
        Map<String,String> headerMap = new LinkedHashMap<>(DEFAULT_HEADER_MAP);
        headerMap.putAll(request.getHeaderMap());
        return headerMap;
    }

    protected List<Response> executeBatchRequests(BatchableRequestBuilder batchableRequestBuilder) throws IOException
    {
        BatchResponseChecker batchResponseChecker = batchResponseCheckerFactory.getBatchResponseChecker(batchableRequestBuilder.getEndpoint());
        if (batchResponseChecker == null) {
            throw new IllegalStateException("No BatchResponseChecker found for endpoint: " + batchableRequestBuilder.getEndpoint());
        }

        List<Response> responseList = new ArrayList<>();

        int batchSize = batchableRequestBuilder.getBatchSize();
        int originalBatchOffset = batchableRequestBuilder.getBatchOffset();
        int currentBatchOffset = originalBatchOffset;

        Response response;
        boolean continueBatchRequesting = true;

        try
        {
            do {
                YahooFinanceRequest batchRequest = batchableRequestBuilder.build();
                response = executeInternal(batchRequest);
                responseList.add(response);

                continueBatchRequesting = batchResponseChecker.isFullBatchResponse(response, batchSize);
                if (continueBatchRequesting)
                {
                    currentBatchOffset = currentBatchOffset + batchSize;
                    batchableRequestBuilder.setBatchOffset(currentBatchOffset);

                    try { Thread.sleep(SLEEP_TIME_BETWEEN_BATCH_REQUESTS); }
                    catch (InterruptedException e) {/* ignore exception */ }
                }
            } while (continueBatchRequesting);
        }
        finally {
            batchableRequestBuilder.setBatchOffset(originalBatchOffset);
        }

        return responseList;
    }
}
