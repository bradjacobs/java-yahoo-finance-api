/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import bwj.yahoofinance.model.request.YahooFinanceRequest;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

public class YahooFinanceClient
{
    private static final String BASE_API_SCHEME = "https";
    private static final String BASE_API_HOST = "query1.finance.yahoo.com";

    private static final String DEFAULT_CONTENT_TYPE = ContentType.APPLICATION_JSON.getMimeType();
    private static final String DEFAULT_USER_AGENT = "Java-Http-Client/11.0.0";


    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    private final CloseableHttpClient httpClient;
    private String contentType;
    private String userAgent;
    private boolean throwExceptionOnHttpError = true;

    public YahooFinanceClient()
    {
        this(createDefaultHttpClient());
    }

    public YahooFinanceClient(CloseableHttpClient httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("HttpClient parameter cannot be null.");
        }
        this.httpClient = httpClient;
        this.contentType = DEFAULT_CONTENT_TYPE;
        this.userAgent = DEFAULT_USER_AGENT;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public void setUserAgent(String userAgent) {
        this.userAgent = userAgent;
    }

    public void setThrowExceptionOnHttpError(boolean throwExceptionOnHttpError) {
        this.throwExceptionOnHttpError = throwExceptionOnHttpError;
    }

    public String executeRequest(YahooFinanceRequest request)
    {
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);
        return execute(url);
    }


    protected String buildRequestUrl(YahooFinanceRequest request)
    {
        Map<String, String> paramMap = request.getParamMap();
        if (paramMap == null) {
            paramMap = Collections.emptyMap();
        }

        String ticker = request.getTicker().toUpperCase();
        YahooEndpoint endpoint = request.getEndpoint();

        URIBuilder builder = new URIBuilder();
        builder.setScheme(BASE_API_SCHEME);
        builder.setHost(BASE_API_HOST);

        //  e.g. /v8/finance/chart/AAPL
        String path = endpoint.getPathPrefix() + "v" + endpoint.getVersion() + "/finance/" + endpoint.getName();

        if (! endpoint.getIsQuery())
        {
            if (endpoint.getSupportsMultipleTickers()) {
                builder.addParameter("symbols", ticker);
            }
            else if (endpoint.getRequiresSymbolParam()) {
                builder.addParameter("symbol", ticker);
            }
            else {
                path +=  "/" + ticker;
            }
        }

        builder.setPath(path);

        for (Map.Entry<String, String> paramEntry : paramMap.entrySet()) {
            builder.addParameter(paramEntry.getKey(), paramEntry.getValue());
        }

        String url = builder.toString();
        return url;
    }



    protected String execute(String url)
    {
        HttpGet httpGet = createGetMethod(url);
        String responseString = "";

        int statusCode = 0;

        // auto-close when declare closable in a try
        try (CloseableHttpResponse response = this.httpClient.execute(httpGet)) {

            HttpEntity entity = response.getEntity();
            if (entity == null) {
                throw new IOException("Unable to read entity response from httpResponse!");
            }

            statusCode = response.getStatusLine().getStatusCode();
            responseString = EntityUtils.toString(entity);

            if (statusCode >= 400 && throwExceptionOnHttpError)
            {
                throw new HttpResponseException(statusCode, responseString);
            }
        }
        catch (Exception e) {
            httpGet.releaseConnection(); // always release connection on error
            throw new RuntimeException(String.format("Unable to fetch content from '%s'. Reason: %s.", url, e.getMessage()), e);
        }

        return responseString;
    }


    protected HttpGet createGetMethod(String url)
    {
        HttpGet httpGet = new HttpGet(url);
        httpGet.setHeader(HttpHeaders.CONTENT_TYPE, this.contentType);
        httpGet.setHeader(HttpHeaders.USER_AGENT, this.userAgent);
        // side note: no need to set "Accept-Encoding" here.
        //    done auto-magically via the 'contentCompressionEnabled' flag inside RequestConfig class
        return httpGet;
    }



    // todo: values are arbitrary
    private static final int MAX_CONNECTIONS_PER_HOST = 10;
    private static final int MAX_TOTAL_CONNECTIONS = 10;
    private static final int CONNECTION_TIMEOUT = 20000;
    private static final int READ_TIMEOUT = 30000;

    private static CloseableHttpClient createDefaultHttpClient()
    {
        RequestConfig config = RequestConfig.custom()
                .setConnectTimeout(CONNECTION_TIMEOUT)
                .setConnectionRequestTimeout(CONNECTION_TIMEOUT)
                .setSocketTimeout(READ_TIMEOUT).build();

        CloseableHttpClient httpClient = HttpClientBuilder.create()
                //.setKeepAliveStrategy() // default should be fine
                .setDefaultRequestConfig(config)
                .setMaxConnPerRoute(MAX_CONNECTIONS_PER_HOST)
                .setMaxConnTotal(MAX_TOTAL_CONNECTIONS)
                .build();

        return httpClient;
    }

}
