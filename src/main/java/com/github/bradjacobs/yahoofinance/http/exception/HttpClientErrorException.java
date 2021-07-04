/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.http.exception;

import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.http.HttpStatus;

/**
 * Exception thrown when an HTTP 4xx is received
 */
public class HttpClientErrorException extends YahooHttpResponseException
{
    public HttpClientErrorException(Response response) {
        super(response);
    }

    //  400, 401, 403, 404, 409, 422: throw MyBusinessException, which contains a message that
    public static HttpClientErrorException createHttpException(Response httpResponse) {

        int statusCode = httpResponse.getCode();

        switch (statusCode) {
            case HttpStatus.SC_BAD_REQUEST: // 400
                return new BadRequestException(httpResponse);
            case HttpStatus.SC_UNAUTHORIZED: // 401
                return new NotAuthorizedException(httpResponse);
            case HttpStatus.SC_FORBIDDEN: // 403
                return new ForbiddenException(httpResponse);
            case HttpStatus.SC_NOT_FOUND: // 404
                return new NotFoundException(httpResponse);
            case HttpStatus.SC_METHOD_NOT_ALLOWED: // 405
                return new NotAllowedException(httpResponse);
            case HttpStatus.SC_NOT_ACCEPTABLE: // 406
                return new NotAcceptableException(httpResponse);
            case HttpStatus.SC_CONFLICT: // 409
                return new ConflictException(httpResponse);
            case HttpStatus.SC_GONE: // 410
                return new GoneException(httpResponse);
            case HttpStatus.SC_UNSUPPORTED_MEDIA_TYPE: // 415
                return new NotSupportedException(httpResponse);
            case HttpStatus.SC_UNPROCESSABLE_ENTITY: // 422
                return new NotProcessableException(httpResponse);
            case 429: // 429
                return new TooManyRequestsException(httpResponse);
            default: // OTHER
                return new HttpClientErrorException(httpResponse);
        }
    }


    // Subclasses for specific HTTP status codes

    /**
     * {@link HttpClientErrorException} for status HTTP 400 Bad Request.
     */
    public static final class BadRequestException extends HttpClientErrorException {
        private BadRequestException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 401 Unauthorized.
     */
    public static final class NotAuthorizedException extends HttpClientErrorException {
        private NotAuthorizedException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 403 Forbidden.
     */
    public static final class ForbiddenException extends HttpClientErrorException {
        private ForbiddenException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 404 Not Found.
     */
    public static final class NotFoundException extends HttpClientErrorException {
        private NotFoundException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 405 Method Not Allowed.
     */
    public static final class NotAllowedException extends HttpClientErrorException {
        private NotAllowedException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 406 Not Acceptable.
     */
    public static final class NotAcceptableException extends HttpClientErrorException {
        private NotAcceptableException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 409 Conflict.
     */
    public static final class ConflictException extends HttpClientErrorException {
        private ConflictException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 410 Gone.
     */
    public static final class GoneException extends HttpClientErrorException {
        private GoneException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 415 Unsupported Media Type.
     */
    public static final class NotSupportedException extends HttpClientErrorException {
        private NotSupportedException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 422 Unprocessable Entity.
     */
    public static final class NotProcessableException extends HttpClientErrorException {
        private NotProcessableException(Response httpResponse) {
            super(httpResponse);
        }
    }

    /**
     * {@link HttpClientErrorException} for status HTTP 429 Too Many Requests.
     */
    public static final class TooManyRequestsException extends HttpClientErrorException {
        private TooManyRequestsException(Response httpResponse) {
            super(httpResponse);
        }
    }
}
