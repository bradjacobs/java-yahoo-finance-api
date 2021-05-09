package bwj.yahoofinance.http;

import org.apache.commons.lang.NotImplementedException;
import org.apache.http.HttpEntity;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.Map;

public class RawApacheClient implements RawHttpClient
{
    private final CloseableHttpClient httpClient;

    public RawApacheClient(CloseableHttpClient httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;
    }

    @Override
    public String executeGet(String url, Map<String, String> requestheaders)
    {
        HttpGet httpGet = createGetMethod(url, requestheaders);
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

            if (statusCode >= 400)
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

    @Override
    public String executePost(String url, String postBody, Map<String, String> requestheaders)
    {
        throw new NotImplementedException("Apache Client POST is not yet implemented");
    }


    protected HttpGet createGetMethod(String url, Map<String,String> headerMap)
    {
        HttpGet httpGet = new HttpGet(url);
        if (headerMap != null) {
            for (Map.Entry<String, String> headerEntry : headerMap.entrySet())
            {
                httpGet.setHeader(headerEntry.getKey(), headerEntry.getValue());
            }

        }
        // side note: no need to set "Accept-Encoding" here.
        //    done auto-magically via the 'contentCompressionEnabled' flag inside RequestConfig class
        return httpGet;
    }

}
