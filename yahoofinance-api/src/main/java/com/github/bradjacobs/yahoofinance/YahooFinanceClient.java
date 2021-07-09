/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.http.exception.HttpExceptionFactory;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterFactory;
import com.github.bradjacobs.yahoofinance.response.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponseConverter;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.validation.YahooRequestValidator;
import org.apache.http.HttpHeaders;
import org.apache.http.client.HttpResponseException;
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

    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;

    // todo: for the moment this is always true.. to fix.
    private final boolean throwExceptionOnHttpError = true;

    private Map<String,String> requestHeaderMap = new LinkedHashMap<>();
    private final BatchableRequestExecutor batchableRequestExecutor;


    public YahooFinanceClient()
    {
        this(HttpClientAdapterFactory.createDefaultClient());
    }

    public YahooFinanceClient(HttpClientAdapter httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }

        setContentType(DEFAULT_CONTENT_TYPE);
        setUserAgent(DEFAULT_USER_AGENT);
        this.httpClient = httpClient;
        this.crumbDataSource = new CrumbDataSource(httpClient);

        this.batchableRequestExecutor = new BatchableRequestExecutor(this);
    }


    public void setContentType(String contentType) {
        requestHeaderMap.put(HttpHeaders.CONTENT_TYPE, contentType);
    }

    public void setUserAgent(String userAgent) {
        requestHeaderMap.put(HttpHeaders.USER_AGENT, userAgent);
    }

    public String executeRequest(YahooFinanceRequest request) throws IOException
    {
        Response response = executeInternal(request);
        return response.getBody();
    }


    public YahooResponse execute(YahooFinanceRequest request) throws IOException
    {
        Response rawResponse = executeInternal(request);
        return new YahooResponse(request.getEndpoint(), rawResponse);
    }

    public YahooBatchResponse executeBatch(YahooFinanceRequest request) throws IOException
    {
        // todo - come back to address this (a litle kludgy)
        if (request instanceof YahooFinanceBatchRequest) {
            return executeBatch((YahooFinanceBatchRequest)request);
        }
        else {
            throw new IllegalArgumentException("Error: cannot execute batch request.  The request is not batchable.");
        }
    }

    public YahooBatchResponse executeBatch(YahooFinanceBatchRequest request) throws IOException
    {
        List<Response> rawResponses = batchableRequestExecutor.executeRequest(request);
        return new YahooBatchResponse(request.getEndpoint(), rawResponses);
    }


    /**
     * Executes request and returns result in a custom list format
     * @param request
     * @return
     * @throws IOException
     */
    // todo - fix terrible method name
    public List<Map<String,Object>> executeListRequest(YahooFinanceRequest request) throws IOException
    {
        Response response = executeInternal(request);
        return convertToListOfMaps(response, request.getEndpoint());
    }

    /**
     * Executes request and returns result in a custom map format
     * @param request
     * @return
     * @throws IOException
     */
    // todo - fix terrible method name
    // todo - may change key just to type 'object'   tbd.
    public Map<String,Map<String,Object>> executeMapRequest(YahooFinanceRequest request) throws IOException
    {
        Response response = executeInternal(request);
        return convertToMapOfMaps(response, request.getEndpoint());
    }





    protected List<Map<String,Object>> convertToListOfMaps(Response response, YahooEndpoint endpoint) throws HttpResponseException
    {
        if (response.isError()) {
            throw HttpExceptionFactory.createException(response);
        }
        YahooResponseConverter responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);
        return responseConverter.convertToListOfMaps(response.getBody());
    }

    protected List<Map<String,Object>> convertToListOfMaps(List<Response> responseList, YahooEndpoint endpoint) throws HttpResponseException
    {
        if (responseList == null || responseList.isEmpty()) {
            return Collections.emptyList();
        }
        else if (responseList.size() == 1) {
            return convertToListOfMaps(responseList.get(0), endpoint);
        }
        else {
            List<Map<String,Object>> totalResults = new ArrayList<>();
            for (Response response : responseList) {
                totalResults.addAll(convertToListOfMaps(response, endpoint));
            }
            return totalResults;
        }
    }


    protected Map<String, Map<String, Object>> convertToMapOfMaps(Response response, YahooEndpoint endpoint) throws HttpResponseException
    {
        if (response.isError()) {
            throw HttpExceptionFactory.createException(response);
        }
        YahooResponseConverter responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);
        return responseConverter.convertToMapOfMaps(response.getBody());
    }

    protected Map<String, Map<String, Object>> convertToMapOfMaps(List<Response> responseList, YahooEndpoint endpoint) throws HttpResponseException
    {
        if (responseList == null || responseList.isEmpty()) {
            return Collections.emptyMap();
        }
        else if (responseList.size() == 1) {
            return convertToMapOfMaps(responseList.get(0), endpoint);
        }
        else {
            Map<String, Map<String, Object>> totalResults = new LinkedHashMap<>(); // todo - tbd which type of map to use
            for (Response response : responseList) {
                totalResults.putAll(convertToMapOfMaps(response, endpoint));
            }
            return totalResults;
        }
    }




    protected Response executeInternal(YahooFinanceRequest request) throws IOException
    {
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);

        Response response = null;
        if (request.isPost()) {
            String postBody = request.getPostBody();
            response = httpClient.executePost(url, postBody, this.requestHeaderMap);
        }
        else {
            response = httpClient.executeGet(url, this.requestHeaderMap);
        }

        if (response.isError() && this.throwExceptionOnHttpError) {
            throw HttpExceptionFactory.createException(response);
        }
        return response;
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
            String crumb = crumbDataSource.getCrumb();
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

    // TODO -- redo/refactor below for handling 'batches'.  the more i look at it the less i li,e it.

    /**
     * Executes the request and sens the results to the given collector.
     *  This is (primarily) used for when you want to do a bunch of batch requests to get 'all' results for a query request.
     * @param request request
     * @param responseCollector collector
     * @throws IOException thrown if something goes wrong.(including 4xx and 5xx) regardless of the throwonexceptoin paraemter
     *     b/c there isn't a good alternative in this case.
     */
    protected void executeCollectionRequest(YahooFinanceRequest request, ResponseCollector responseCollector) throws IOException
    {
        BatchableRequestStrategy batchableRequestStrategy = request.getBatchableRequestStrategy();
        if (batchableRequestStrategy == null) {
            // normal single request
            Response response = executeInternal(request);
            if (response.isError()) {
                throw HttpExceptionFactory.createException(response);
            }
            responseCollector.constructObjectCollections(response.getBody());
        }
        else
        {
            //   todo
            // no longer implemented... to remove.
        }
    }

    private static void batchIterationSleep() {
        try { Thread.sleep(SLEEP_TIME_BETWEEN_BATCH_REQUESTS); }
        catch (InterruptedException e) {/* ignore */ }
    }

    // pseudo-observer to avoid redundant batching code.
    //
    private static abstract class ResponseCollector
    {
        protected final YahooResponseConverter responseConverter;

        protected ResponseCollector(YahooEndpoint endpoint) {
            this.responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);
        }

        public abstract int constructObjectCollections(String json);
    }

    private static class ListOfMapsResponseCollector extends ResponseCollector
    {
        private final List<Map<String, Object>> totalResults = new ArrayList<>();

        public ListOfMapsResponseCollector(YahooEndpoint endpoint) {
            super(endpoint);
        }

        public int constructObjectCollections(String json) {
            List<Map<String, Object>> batchResponse = responseConverter.convertToListOfMaps(json);
            this.totalResults.addAll(batchResponse);
            return batchResponse.size();
        }

        public List<Map<String, Object>> getTotalResults() {
            return totalResults;
        }
    }

    private static class MapOfMapsResponseCollector extends ResponseCollector
    {
        private final Map<String,Map<String,Object>> totalResults = new LinkedHashMap<>();  // todo - which kind of map

        public MapOfMapsResponseCollector(YahooEndpoint endpoint) {
            super(endpoint);
        }

        public int constructObjectCollections(String json) {
            Map<String,Map<String,Object>> batchResponse = responseConverter.convertToMapOfMaps(json);
            this.totalResults.putAll(batchResponse);
            return batchResponse.size();
        }

        public Map<String, Map<String, Object>> getTotalResults() {
            return totalResults;
        }
    }

}
