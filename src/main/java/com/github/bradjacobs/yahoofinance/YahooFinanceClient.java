/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.builder.BatchableRequestStrategy;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterFactory;
import com.github.bradjacobs.yahoofinance.response.YahooResponseConverter;
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

    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    // allow a very brief pause b/w each batch request for philanthropy.
    private static final long SLEEP_TIME_BETWEEN_BATCH_REQUESTS = 100L;

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;

    private Map<String,String> requestHeaderMap = new LinkedHashMap<>();


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

    /**
     * Executes request and returns result in a custom list format
     * @param request
     * @return
     * @throws IOException
     */
    // todo - fix terrible method name
    public List<Map<String,Object>> executeListRequest(YahooFinanceRequest request) throws IOException
    {
        ListOfMapsResponseCollector listOfMapsResponseCollector = new ListOfMapsResponseCollector(request.getEndpoint());
        executeCollectionRequest(request, listOfMapsResponseCollector);
        return listOfMapsResponseCollector.getTotalResults();
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
        MapOfMapsResponseCollector mapOfMapsResponseCollector = new MapOfMapsResponseCollector(request.getEndpoint());
        executeCollectionRequest(request, mapOfMapsResponseCollector);
        return mapOfMapsResponseCollector.getTotalResults();
    }


    protected Response executeInternal(YahooFinanceRequest request) throws IOException
    {
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);

        Response response = null;
        if (request.isPost())
        {
            String postBody = request.getPostBody();
            response = httpClient.executePost(url, postBody, this.requestHeaderMap);
        }
        else
        {
            response = httpClient.executeGet(url, this.requestHeaderMap);
        }

        if (response.isError()) {
            // TODO - come back and handle better
            throw new RuntimeException("Error occurred during request: ");
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



    protected void executeCollectionRequest(YahooFinanceRequest request, ResponseCollector responseCollector) throws IOException
    {
        BatchableRequestStrategy batchableRequestStrategy = request.getBatchableRequestStrategy();
        if (batchableRequestStrategy == null) {
            // normal single request
            String responseJson = executeRequest(request);
            responseCollector.constructObjectCollections(responseJson);
        }
        else
        {
            int generatedObjectCount = -1;
            int configuredBatchSize = batchableRequestStrategy.getBatchSize();
            do {
                YahooFinanceRequest batchRequest = batchableRequestStrategy.buildNewRequest();

                String responseJson = executeRequest(batchRequest);
                generatedObjectCount = responseCollector.constructObjectCollections(responseJson);
                batchableRequestStrategy.incrementBatchOffset();
                if (configuredBatchSize == generatedObjectCount) {
                    batchIterationSleep();
                }
            } while (configuredBatchSize == generatedObjectCount);
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
            this.responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);;
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


    /*
        TODO - originally pondering of doing a builder for client creation,
              but temporarily having 2nd thoughts.  may eventually do it... TBD

        public static Builder builder() {
            return new Builder();
        }

        public static class Builder
        {
            private static final String DEFAULT_CONTENT_TYPE = "application/json";
            private static final String DEFAULT_USER_AGENT = "Java-Http-Client/11.0.0";

            private String defaultContentType = DEFAULT_CONTENT_TYPE;
            private String defaultUserAgent = DEFAULT_USER_AGENT;
            private boolean throwExceptionOnHttpError = true;
            private boolean prettyJsonEnabled = false;
            //private boolean flattenRawResponses = false;
            private HttpClientAdapter httpClientAdapter = HttpClientAdapterFactory.createDefaultClient();

            private Builder() {}

            public YahooFinanceClient.Builder setDefaultContentType(String defaultContentType) {
                this.defaultContentType = defaultContentType;
                return this;
            }
            public YahooFinanceClient.Builder setDefaultUserAgent(String defaultUserAgent) {
                this.defaultUserAgent = defaultUserAgent;
                return this;
            }
            public YahooFinanceClient.Builder setExceptionOnHttpError(boolean exceptionOnHttpError) {
                this.throwExceptionOnHttpError = exceptionOnHttpError;
                return this;
            }
            public YahooFinanceClient.Builder setUsePrettyJson(boolean usePrettyJson) {
                this.prettyJsonEnabled = usePrettyJson;
                return this;
            }
    //        public YahooFinanceClient.Builder setAlwaysFlattenRawResposne(boolean flattenRawResponses) {
    //            this.flattenRawResponses = flattenRawResponses;
    //            return this;
    //        }
            public YahooFinanceClient.Builder setHttpClientAdapter(HttpClientAdapter httpClientAdapter) {
                this.httpClientAdapter = httpClientAdapter;
                return this;
            }

            public YahooFinanceClient build() {
                return new YahooFinanceClient(this);
            }
        }
     */

}
