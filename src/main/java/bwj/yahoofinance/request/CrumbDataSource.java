/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;

import bwj.yahoofinance.http.HttpClientAdapter;
import bwj.yahoofinance.http.Response;

import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

/**
 * Used to fetch a yahoo finance 'crumb' value.
 *
 *   Will Lazy Load crumb value and cache it for a limited period of time.
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
    private String parseOutCrumb(String response) {
        int crumbIndexStart = response.indexOf(CRUMB_REPSONSE_INTRO) + CRUMB_REPSONSE_INTRO.length();
        int crumbIndexEnd = response.indexOf("\"", crumbIndexStart+1);
        String crumbValue = response.substring(crumbIndexStart, crumbIndexEnd);


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