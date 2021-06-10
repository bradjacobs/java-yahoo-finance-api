/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpClientAdapterFactory;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.CrumbDataSource;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterFactory;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.validation.YahooRequestValidator;
import org.apache.http.client.utils.URIBuilder;

import java.io.IOException;
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

    private Map<String,String> requestHeaderMap = new LinkedHashMap<>();

    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    // httpClient is a simple interface around the 'true' httpClient.
    private final HttpClientAdapter httpClient;
    private final CrumbDataSource crumbDataSource;


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
        requestHeaderMap.put("Content-Type", contentType);
    }

    public void setUserAgent(String userAgent) {
        requestHeaderMap.put("User-Agent", userAgent);
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
        String responseJson = executeRequest(request);
        return ResponseConverterFactory.getResponseConverter(request.getEndpoint()).convertToListOfMaps(responseJson);
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
        String responseJson = executeRequest(request);
        return ResponseConverterFactory.getResponseConverter(request.getEndpoint()).convertToMapOfMaps(responseJson);
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
            updatedParamMap.put("crumb", crumb);
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
