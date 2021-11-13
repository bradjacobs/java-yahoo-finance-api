/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseChecker;
import com.github.bradjacobs.yahoofinance.response.batch.BatchResponseCheckerFactory;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.http.exception.HttpExceptionFactory;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.batch.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponseGenerator;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.validation.YahooRequestValidator;
import org.apache.http.HttpHeaders;
import org.apache.http.client.utils.URIBuilder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class YahooFinanceClient
{
    private static final String BASE_API_SCHEME = "https";
    private static final String BASE_API_HOST = "query1.finance.yahoo.com";

    private static final String DEFAULT_CONTENT_TYPE = "application/json";
    private static final String DEFAULT_USER_AGENT = "Java-Http-Client/11.0.0";

    private static final String CRUMB_KEY = "crumb";

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;
    private final BatchResponseCheckerFactory batchResponseCheckerFactory = new BatchResponseCheckerFactory();

    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;

    // todo: for the moment this is always true.. to fix.
    private final boolean throwExceptionOnHttpError = true;

    private final Map<String,String> defaultRequestHeaderMap = new LinkedHashMap<>();
    private final YahooResponseGenerator yahooResponseGenerator = new YahooResponseGenerator();



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
        this.initializeDefaultHeaderMap();
    }

    private void initializeDefaultHeaderMap() {
        defaultRequestHeaderMap.put(HttpHeaders.CONTENT_TYPE, DEFAULT_CONTENT_TYPE);
        defaultRequestHeaderMap.put(HttpHeaders.USER_AGENT, DEFAULT_USER_AGENT);
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
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);
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
        Map<String,String> headerMap = new LinkedHashMap<>(defaultRequestHeaderMap);
        headerMap.putAll(request.getHeaderMap());
        return headerMap;
    }


    protected String buildRequestUrl(YahooFinanceRequest request) throws IOException
    {
        Map<String, String> paramMap = request.getParamMap();
        if (paramMap == null) {
            paramMap = Collections.emptyMap();
        }

        String ticker = request.getTicker().toUpperCase();
        YahooEndpoint endpoint = request.getEndpoint();

        if (endpoint.isCrumbRequest())
        {
            String crumb = crumbDataSource.getCrumb(request);
            Map<String,String> updatedParamMap = new LinkedHashMap<>(paramMap);
            updatedParamMap.put(CRUMB_KEY, crumb);
            paramMap = updatedParamMap;
        }

        URIBuilder builder = new URIBuilder();
        builder.setScheme(BASE_API_SCHEME);
        builder.setHost(BASE_API_HOST);

        //  e.g. /v8/finance/chart/AAPL
        String path = endpoint.getPathPrefix() + "v" + endpoint.getVersion() + "/finance/" + endpoint.getName();

        // check if need to append the ticker to the path itself.
        if ( endpoint.isTickerOnPath() )
        {
            path +=  "/" + ticker;
        }

        builder.setPath(path);

        for (Map.Entry<String, String> paramEntry : paramMap.entrySet()) {
            builder.addParameter(paramEntry.getKey(), paramEntry.getValue());
        }

        return builder.toString();
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
                    batchIterationSleep();
                }
            } while (continueBatchRequesting);
        }
        finally
        {
            batchableRequestBuilder.setBatchOffset(originalBatchOffset);
        }

        return responseList;
    }

    private void batchIterationSleep() {
        try { Thread.sleep(SLEEP_TIME_BETWEEN_BATCH_REQUESTS); }
        catch (InterruptedException e) {/* ignore */ }
    }

}
