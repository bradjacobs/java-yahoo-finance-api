/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.http.exception;

import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.http.client.HttpResponseException;

public class HttpExceptionFactory
{
    private HttpExceptionFactory() {}

    public static HttpResponseException createException(Response httpResponse)
    {
        int statusCode = httpResponse.getCode();
        if (statusCode >= 400 && statusCode <= 499) {
            return HttpClientErrorException.createHttpException(httpResponse);
        }
        else if (statusCode >= 500 && statusCode <= 599) {
            return HttpServerErrorException.createHttpException(httpResponse);
        }
        else {
            throw new IllegalArgumentException("Unable to create HttpStatusCodeException: Unhandled status code: " + statusCode);
        }
    }

}
