package com.github.bradjacobs.yahoofinance.http;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.Map;

/*
  TODO - Post can cause a Warning like the following:
     WARNING: Invalid cookie header: ....
  which probably just means a client configuration tweak, but it's a lower priority at present.
 */
public class HttpCommonsClientAdapter implements HttpClientAdapter
{
    private static final ContentType BODY_CONTENT_TYPE = ContentType.APPLICATION_JSON;
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
        return executeRequest( createGetMethod(url, requestheaders) );
    }


    @Override
    public Response executePost(String url, String postBody, Map<String, String> requestheaders) throws IOException
    {
        return executeRequest( createPostMethod(url, postBody, requestheaders) );
    }


    public Response executeRequest(HttpRequestBase httpMethod) throws IOException
    {
        // auto-close when declare closable in a try
        try (CloseableHttpResponse response = this.httpClient.execute(httpMethod)) {

            return createGenericResponse(response);
        }
        catch (IOException e) {
            httpMethod.releaseConnection(); // always release connection IFF error  (note: arguably isn't really necessary)
            throw e;
        }
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


    // note:  org.apache.http.client.methods.RequestBuilder is available if need more advanced construction below.

    private HttpGet createGetMethod(String url, Map<String,String> headerMap) {
        HttpGet httpGet = new HttpGet(url);
        addInHeaders(httpGet, headerMap);
        return httpGet;
    }

    private HttpPost createPostMethod(String url, String body, Map<String,String> headerMap) {
        HttpPost httpPost = new HttpPost(url);
        if (body != null) {
            httpPost.setEntity(new StringEntity(body, BODY_CONTENT_TYPE));
        }
        addInHeaders(httpPost, headerMap);
        return httpPost;
    }

    private void addInHeaders(HttpRequestBase httpMethod, Map<String,String> headerMap) {
        if (headerMap != null) {
            for (Map.Entry<String, String> headerEntry : headerMap.entrySet()) {
                httpMethod.setHeader(headerEntry.getKey(), headerEntry.getValue());
            }
        }
        // side note: "Accept-Encoding" auto-magically via the 'contentCompressionEnabled' flag
        //  inside RequestConfig class.  (few other headers set as well)
    }

}
