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
  TODO - Post can cause a Warning like the following:    WARNING: Invalid cookie header: ....
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
    public Response executeGet(String url, Map<String, String> requestHeaders) throws IOException
    {
        return executeRequest( createRequest(url, requestHeaders, null) );
    }

    @Override
    public Response executePost(String url, String postBody, Map<String, String> requestHeaders) throws IOException
    {
        return executeRequest( createRequest(url, requestHeaders, postBody) );
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

        Response.Builder builder = Response.builder()
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

    private HttpRequestBase createRequest(String url, Map<String,String> requestHeaders, String postBody)
    {
        // NOTE: there is a builder available (org.apache.http.client.methods.RequestBuilder)
        //    but it's still kinda ugly with that as well !!
        HttpRequestBase httpMethod;
        if (postBody != null) {
            HttpPost httpPost = new HttpPost(url);
            httpPost.setEntity(new StringEntity(postBody, BODY_CONTENT_TYPE));
            httpMethod = httpPost;
        }
        else {
            httpMethod = new HttpGet(url);
        }
        if (requestHeaders != null) {
            for (Map.Entry<String, String> headerEntry : requestHeaders.entrySet()) {
                // side note: "Accept-Encoding" auto-magically via the 'contentCompressionEnabled' flag
                //  inside RequestConfig class.  (a few other headers set as well)
                httpMethod.setHeader(headerEntry.getKey(), headerEntry.getValue());
            }
        }
        return httpMethod;
    }
}
