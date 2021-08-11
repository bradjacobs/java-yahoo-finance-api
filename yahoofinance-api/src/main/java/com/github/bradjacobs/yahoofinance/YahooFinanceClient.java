/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.http.exception.HttpClientErrorException;
import com.github.bradjacobs.yahoofinance.http.exception.HttpExceptionFactory;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.YahooBatchResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponse;
import com.github.bradjacobs.yahoofinance.response.YahooResponseGenerator;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.validation.YahooRequestValidator;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpHeaders;
import org.apache.http.client.utils.URIBuilder;

import javax.security.auth.login.LoginException;
import java.io.IOException;
import java.security.GeneralSecurityException;
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

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;

    // todo: for the moment this is always true.. to fix.
    private final boolean throwExceptionOnHttpError = true;

    private Map<String,String> requestHeaderMap = new LinkedHashMap<>();
    private final BatchableRequestExecutor batchableRequestExecutor;
    private final YahooResponseGenerator yahooResponseGenerator = new YahooResponseGenerator();


    public YahooFinanceClient()
    {
        this(HttpClientAdapterFactory.createDefaultClient());
    }

    public YahooFinanceClient(String userName, String password)
    {
        this(HttpClientAdapterFactory.createDefaultClient(), userName, password);
    }

    public YahooFinanceClient(HttpClientAdapter httpClient)
    {
        this(httpClient, null, null);
    }


    public YahooFinanceClient(HttpClientAdapter httpClient, String userName, String password)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }

        if (StringUtils.isNotEmpty(userName) && StringUtils.isNotEmpty(password))
        {
            // todo - do smarter exception handling!!
            YahooLoginExecutor yahooLoginExecutor = new YahooLoginExecutor(httpClient);
            try {
                yahooLoginExecutor.doLogin(userName, password);
            }
            catch (Exception e) {
                throw new RuntimeException("Unable to login: " + e.getMessage(), e);  // todo better exception needed!!
            }
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


    public YahooResponse execute(YahooFinanceRequest request) throws IOException
    {
        Response rawResponse = executeInternal(request);
        return yahooResponseGenerator.makeResposne(request, rawResponse);
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
        return yahooResponseGenerator.makeBatchResponse(request, rawResponses);
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
}
