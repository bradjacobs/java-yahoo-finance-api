/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

/**
 * Used to fetch a yahoo finance 'crumb' value.
 *
 *   Will Lazy Load crumb value and cache it for a limited period of time.
 */
/*
  TO RESEARCH:
     The following URL is available to directly get a crumb value:
         https://query2.finance.yahoo.com/v1/test/getcrumb
     However, when called via the API it will return a blank  (but will work in a browser)
       there's probably a 'magic header' that makes it work.
 */
public class CrumbDataSource
{
    private final HttpClientAdapter httpClient;

    // url to use to get a 'crumb' value from the response.
    private static final String URL = "https://finance.yahoo.com/quote/AAPL/history?p=AAPL";
    private static final Map<String,String> requestHeaders = Collections.singletonMap("Content-Type", "application/x-www-form-urlencoded");
    private static final long EXPIRATION_TIME = 1000 * 60 * 60 * 4; // (4 hours) - max time to cache a crumb value

    // look for this string in the response body to 'locate' the crumb value.
    private static final String CRUMB_REPSONSE_INTRO = "\"CrumbStore\":{\"crumb\":\"";


    private CrumbObject crumbObject = null;



    public CrumbDataSource(HttpClientAdapter httpClient)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("httpClient cannot be null.");
        }
        this.httpClient = httpClient;
    }


    public String getCrumb() throws IOException
    {
        //  not thread safe!
        if (crumbObject == null || crumbObject.isExpired()) {
            String crumbValue = reloadCrumb();
            crumbObject = new CrumbObject(crumbValue);
        }
        return crumbObject.getCrumb();
    }




    private String reloadCrumb() throws IOException
    {
        Response response = httpClient.executeGet(URL, requestHeaders);
        if (response.getCode() == 200) {
            return parseOutCrumb(response.getBody());
        }

        throw new RuntimeException("TODO: implement error handling + retry logic");
    }

    /**
     * parse out the actual crumb value from a full http response body
     * @param response http response body
     * @return crumb
     */
    private String parseOutCrumb(String response)
    {
        String crumbValue = StringUtils.substringBetween(response, CRUMB_REPSONSE_INTRO, "\"");

        // sometimes the crumb will contain a LITERAL substring "\u002F"
        //   (instead of a slash).  Thus need minor cleanup for this scenario.
        crumbValue = crumbValue.replace("\\u002F", "/");
        crumbValue = crumbValue.replace("\\u002f", "/");

        return crumbValue;
    }


    private static class CrumbObject {
        private final String crumb;
        private final Date creationTime;
        private Date lastAccessTime;

        public CrumbObject(String crumb)
        {
            this.crumb = crumb;
            this.creationTime = new Date();
            this.lastAccessTime = new Date();
        }

        private String getCrumb() {
            this.lastAccessTime = new Date();
            return crumb;
        }

        private boolean isExpired() {
            long timeNow = System.currentTimeMillis();
            long createTime = creationTime.getTime();

            if (createTime + EXPIRATION_TIME < timeNow) {
                return true;
            }

            return false;
        }
    }

}
