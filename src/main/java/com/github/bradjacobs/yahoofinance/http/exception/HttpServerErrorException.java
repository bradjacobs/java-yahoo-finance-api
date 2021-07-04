/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.http.exception;

import com.github.bradjacobs.yahoofinance.http.Response;


/**
 * Exception thrown when an HTTP 5xx is received
 */
public class HttpServerErrorException extends YahooHttpResponseException
{
    public HttpServerErrorException(Response response) {
        super(response);
    }


    public static HttpServerErrorException createHttpException(Response httpResponse)
    {
        int statusCode = httpResponse.getCode();

        switch (statusCode) {
            case org.apache.http.HttpStatus.SC_INTERNAL_SERVER_ERROR: // 500
                return new InternalServerException(httpResponse);
            case org.apache.http.HttpStatus.SC_NOT_IMPLEMENTED:       // 501
                return new NotImplementedException(httpResponse);
            case org.apache.http.HttpStatus.SC_BAD_GATEWAY:           // 502
                return new BadGatewayException(httpResponse);
            case org.apache.http.HttpStatus.SC_SERVICE_UNAVAILABLE:   // 503
                return new ServiceUnavailableException(httpResponse);
            case org.apache.http.HttpStatus.SC_GATEWAY_TIMEOUT:       // 504
                return new GatewayTimeoutException(httpResponse);
            default: // OTHER
                return new HttpServerErrorException(httpResponse);
        }
    }

    // Subclasses for specific HTTP status codes

    /**
     * {@link HttpServerErrorException} for status HTTP 500 Internal Server Error.
     */
    public static final class InternalServerException extends HttpServerErrorException {
        private InternalServerException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpServerErrorException} for status HTTP 501 Not Implemented.
     */
    public static final class NotImplementedException extends HttpServerErrorException {
        private NotImplementedException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpServerErrorException} for status HTTP 502 Bad Gateway.
     */
    public static final class BadGatewayException extends HttpServerErrorException {
        private BadGatewayException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpServerErrorException} for status HTTP 503 Service Unavailable.
     */
    public static final class ServiceUnavailableException extends HttpServerErrorException {
        private ServiceUnavailableException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpServerErrorException} for status HTTP 504 Gateway Timeout.
     */
    public static final class GatewayTimeoutException extends HttpServerErrorException {
        private GatewayTimeoutException(Response httpResponse) {
            super(httpResponse);
        }
    }
}
