/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.ContentType;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class YahooFinanceClient
{
    private static final String BASE_URL = "https://query1.finance.yahoo.com/";

    private static final String DEFAULT_CONTENT_TYPE = ContentType.APPLICATION_JSON.getMimeType();
    private static final String DEFAULT_USER_AGENT = "Java-Http-Client/11.0.0";


    public static void main(String[] args) {

        PrivilegedAction<String> pa = () -> System.getProperty("java.version");
        String version = AccessController.doPrivileged(pa);
        String sss =  "Java-http-client/" + version;

        int kjkj = 333;


    }

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

    public String executeRequest(String ticker, YahooEndpoint endpoint)
    {
        return executeRequest(ticker, endpoint, null);

    }
    public String executeRequest(String ticker, YahooEndpoint endpoint, Map<String,String> paramMap)
    {
        String url = buildRequestUrl(ticker, endpoint, paramMap);
        return execute(url);
    }


    protected String buildRequestUrl(String ticker,  YahooEndpoint endpoint, Map<String,String> paramMap)
    {
        StringBuilder sb = new StringBuilder();
        sb.append(BASE_URL);

        String endpointPath = endpoint.getPath();

        // to confirm: all requests are for exactly 1 ticker symbol?
        if (StringUtils.isEmpty(ticker)) {
            throw new IllegalArgumentException("Must provide a ticker as part of the request.");
        }

        if (endpointPath.contains("%s")) {
            endpointPath = String.format(endpointPath, ticker);
            sb.append(endpointPath);
        }
        else {
            sb.append(endpointPath);
            sb.append('/');
            sb.append(ticker);
        }

        if (paramMap != null && paramMap.size() > 0)
        {
            List<NameValuePair> nvpList = new ArrayList<>(paramMap.size());
            for (Map.Entry<String, String> entry : paramMap.entrySet()) {
                nvpList.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
            }

            String paramString = URLEncodedUtils.format(nvpList, HTTP.DEF_CONTENT_CHARSET.name());

            if (! sb.toString().contains("?")) {
                sb.append('?');
            }
            else {
                sb.append('&');
            }
            sb.append(paramString);
        }

        String url = sb.toString();
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
