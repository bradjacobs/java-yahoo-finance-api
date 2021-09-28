package com.github.bradjacobs.yahoofinance.http;

import okhttp3.*;

import java.io.IOException;
import java.util.Map;

public class OkHttpClientAdapter implements HttpClientAdapter
{
    // note: currently 'assuming' all posts will be of type JSON.
    private static final MediaType JSON_TYPE = MediaType.parse("application/json; charset=utf-8");


    private final OkHttpClient httpClient;

    public OkHttpClientAdapter(OkHttpClient httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;
    }

    @Override
    public Response executeGet(String url, Map<String, String> requestheaders) throws IOException
    {
        Request request = createRequest(url, requestheaders, null);
        return executeRequest(request);
    }


    @Override
    public Response executePost(String url, String postBody, Map<String, String> requestheaders) throws IOException
    {
        Request request = createRequest(url, requestheaders, postBody);
        return executeRequest(request);
    }


    protected Response executeRequest(Request request) throws IOException
    {
        try (okhttp3.Response okResponse = httpClient.newCall(request).execute())
        {
            return createGenericResponse(okResponse);
        }
    }

    private Response createGenericResponse(okhttp3.Response okResponse) throws IOException
    {
        Response.Builder builder
            = new Response.Builder()
            .code(okResponse.code())
            .message(okResponse.message())
            .body( (okResponse.body() != null ? okResponse.body().string() : null) );

        Headers headers = okResponse.headers();
        if (headers != null) {
            for (String name : headers.names()) {
                builder.withHeader(name, headers.get(name));
            }
        }

        return builder.build();
    }

    private Request createRequest(String url, Map<String,String> headerMap, String postBody)
    {
        Request.Builder builder = new Request.Builder().url(url);
        if (postBody != null) {
            builder.post(RequestBody.create(postBody, JSON_TYPE));
        }
        if (headerMap != null) {
            for (Map.Entry<String, String> entry : headerMap.entrySet()) {
                builder.addHeader(entry.getKey(), entry.getValue());
            }
        }

        return builder.build();
    }

}
