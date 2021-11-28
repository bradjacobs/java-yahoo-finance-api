/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.http.exception.HttpExceptionFactory;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Used to fetch a yahoo finance 'crumb' value.
 * Will Lazy Load crumb value and cache it for a limited period of time.
 */
public class CrumbDataSource
{
    // url to use to get a 'crumb' value from the response.
    private static final String YAHOO_PAGE_URL = "https://finance.yahoo.com/quote/AAPL/profile?p=AAPL";
    private static final String DIRECT_JSON_URL = "https://query2.finance.yahoo.com/v1/test/getcrumb";  // only works if 'correct' headers available

    private static final Map<String,String> WEB_REQUEST_HEADERS = Collections.singletonMap("Content-Type", "application/x-www-form-urlencoded");
    private static final long EXPIRATION_TIME = 1000 * 60 * 60 * 4; // (4 hours) - max time to cache a crumb value

    // look for this string in the response body to 'locate' the crumb value.
    private static final String CRUMB_RESPONSE_INTRO = "\"CrumbStore\":{\"crumb\":\"";
    private static final String COOKIE_HEADER_NAME = "Cookie";

    private final HttpClientAdapter httpClient;
    private final AtomicLong expirationTime = new AtomicLong(0L);
    private String crumbValue = "";

    public CrumbDataSource(HttpClientAdapter httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;
    }

    public String getCrumb(YahooRequest request) throws IOException
    {
        if (isExpired()) {
            // super-simplified-sync
            synchronized (this) {
                if (isExpired()) {
                    String newCrumbValue;
                    try {
                        newCrumbValue = reloadCrumb(request);
                    }
                    catch (IOException e) {
                        throw new UncheckedIOException(e);
                    }
                    if (StringUtils.isNotEmpty(newCrumbValue)) {
                        crumbValue = newCrumbValue;
                        expirationTime.set(System.currentTimeMillis() + EXPIRATION_TIME);
                    }
                }
            }
        }
        return crumbValue;
    }

    private boolean isExpired() {
        long timeNow = System.currentTimeMillis();
        if (timeNow >= expirationTime.longValue()) {
            return true;
        }
        return false;
    }

    /**
     * Makes a network call to get (or reload) a crumb value.
     * @return crumb string
     * @throws IOException exception
     */
    private String reloadCrumb(YahooRequest request) throws IOException
    {
        String newCrumbValue = "";
        Map<String, String> requestHeaderMap = request.getHeaderMap();
        if (requestHeaderMap == null) {
            requestHeaderMap = Collections.emptyMap();
        }

        // do direct api call if have successfully had crumb value before OR have a cookie header value.
        if (StringUtils.isNotEmpty(this.crumbValue) || requestHeaderMap.containsKey(COOKIE_HEADER_NAME)) {
            newCrumbValue = getCrumbViaApi(requestHeaderMap);
        }

        if (StringUtils.isEmpty(newCrumbValue)) {
            // usually the very first crumb request must be called this way in order to 'prime'
            //   the httpClient with correct headers for future requests.
            newCrumbValue = getCrumbViaWebPage();
        }

        if (StringUtils.isEmpty(newCrumbValue)) {
            throw new IllegalStateException("Unable to fetch crumb value.");
        }
        return newCrumbValue;
    }

    private String getCrumbViaApi(Map<String, String> requestHeaderMap) throws IOException
    {
        Map<String,String> customHeaderMap = new LinkedHashMap<>();
        customHeaderMap.put("Content-Type", "application/json");
        if (requestHeaderMap.containsKey(COOKIE_HEADER_NAME)) {
            customHeaderMap.put(COOKIE_HEADER_NAME, requestHeaderMap.get(COOKIE_HEADER_NAME));
        }
        Response response = httpClient.executeGet(DIRECT_JSON_URL, customHeaderMap);
        if (response.isError()) {
            throw HttpExceptionFactory.createException(response);
        }
        return response.getBody();
    }

    private String getCrumbViaWebPage() throws IOException
    {
        Response response = httpClient.executeGet(YAHOO_PAGE_URL, WEB_REQUEST_HEADERS);
        if (response.isError()) {
            throw HttpExceptionFactory.createException(response);
        }
        return parseOutCrumb(response.getBody());
    }

    /**
     * parse out the actual crumb value from a full http response body
     * @param response http response body
     * @return crumb
     */
    private String parseOutCrumb(String response)
    {
        String crumbValue = StringUtils.substringBetween(response, CRUMB_RESPONSE_INTRO, "\"");

        // sometimes the crumb will contain a LITERAL substring "\u002F"
        //   (instead of a slash).  Thus need minor cleanup for this scenario.
        crumbValue = crumbValue.replace("\\u002F", "/");
        crumbValue = crumbValue.replace("\\u002f", "/");

        return crumbValue;
    }
}
