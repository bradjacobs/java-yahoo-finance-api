/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.http.exception;

import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.http.client.HttpResponseException;

import java.util.Map;

/**
 * exception class that holds all useful information from an http error response (i.e. 4xx and 5xx)
 */
/*
  note:  spring also hase some similar classes, but not sure want to pull in spring dependency just for that reason.
 */
public class YahooHttpResponseException extends HttpResponseException
{
    private final String responseBody;
    private final Map<String,String> responseHeaders;
    public YahooHttpResponseException(Response response)
    {
        super(response.getCode(), response.getMessage());
        this.responseBody = response.getBody();
        this.responseHeaders = response.getHeaderMap();
    }

    public String getResponseBody() {
        return this.responseBody;
    }

    public Map<String,String> getResponseHeaders() {
        return this.responseHeaders;
    }
}
