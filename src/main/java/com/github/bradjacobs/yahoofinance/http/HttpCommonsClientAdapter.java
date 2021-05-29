package com.github.bradjacobs.yahoofinance.http;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.Map;

public class HttpCommonsClientAdapter implements HttpClientAdapter
{
    private final CloseableHttpClient httpClient;

    public HttpCommonsClientAdapter(CloseableHttpClient httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;
    }

    @Override
    public Response executeGet(String url, Map<String, String> requestheaders) throws IOException
    {
        HttpGet httpGet = createGetMethod(url, requestheaders);

        // auto-close when declare closable in a try
        try (CloseableHttpResponse response = this.httpClient.execute(httpGet)) {

            return createGenericResponse(response);
        }
        catch (IOException e) {
            httpGet.releaseConnection(); // always release connection on error
            throw e;
        }
    }


    @Override
    public Response executePost(String url, String postBody, Map<String, String> requestheaders)
    {
        throw new NotImplementedException("Apache Client POST is not yet implemented");
    }


    private Response createGenericResponse(CloseableHttpResponse response) throws IOException
    {
        HttpEntity entity = response.getEntity();
        if (entity == null) {
            throw new IOException("Unable to read entity response from httpResponse!");
        }

        Response.Builder builder
            = new Response.Builder()
                .code(response.getStatusLine().getStatusCode())
                .message(response.getStatusLine().getReasonPhrase())
                .body( EntityUtils.toString(entity) );

        Header[] headers = response.getAllHeaders();
        if (headers != null) {
            for (Header header : headers) {
                builder.withHeader(header.getName(), header.getValue());
            }
        }

        return builder.build();
    }


    private HttpGet createGetMethod(String url, Map<String,String> headerMap)
    {
        HttpGet httpGet = new HttpGet(url);
        if (headerMap != null) {
            for (Map.Entry<String, String> headerEntry : headerMap.entrySet()) {
                httpGet.setHeader(headerEntry.getKey(), headerEntry.getValue());
            }
        }
        // side note: "Accept-Encoding" auto-magically via the 'contentCompressionEnabled' flag
        //  inside RequestConfig class.  (few other headers set as well)
        return httpGet;
    }

}
