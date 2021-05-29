package com.github.bradjacobs.yahoofinance.http;

import java.io.IOException;
import java.util.Map;

public interface HttpClientAdapter
{
    default
    Response executeGet(String url) throws IOException {
        return executeGet(url, null);
    };

    Response executeGet(String url, Map<String,String> requestheaders) throws IOException;


    default
    Response executePost(String url, String postBody)  throws IOException {
        return executePost(url, postBody, null);
    };

    Response executePost(String url, String postBody, Map<String,String> requestheaders) throws IOException;

}
