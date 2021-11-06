package com.github.bradjacobs.yahoofinance.batch;

import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.commons.lang3.StringUtils;

public class CountTotalPrefixBatchResponseChecker implements FullBatchResponseChecker
{
    private static final String COUNT_PREFIX = "\"count\":";
    private static final String TOTAL_PREFIX = "\"total\":";
    private static final int RESPONSE_INTRO_SIZE = 300; // how much of the first part of the response to analyze.

    @Override
    public boolean isFullBatchResponse(Response response, int batchSize)
    {
        if (response == null || response.isError()) {
            return false;
        }

        // Note: not interested in parsing out HUGE response body, thus just grab the 'first part' of the response
        //   to determine the information required.
        String responseBody = response.getBody();
        String responseBodySubstring = responseBody.substring(0, Math.min(responseBody.length(), RESPONSE_INTRO_SIZE));

        int count = 0;
        int total = 0;

        try {
            count = Integer.parseInt( StringUtils.substringBetween(responseBodySubstring, COUNT_PREFIX, ",") );
            total = Integer.parseInt( StringUtils.substringBetween(responseBodySubstring, TOTAL_PREFIX, ",") );
        }
        catch (Exception e) {
            /* ignore (for now) */
        }

        if (count == 0 || total == 0 || (count != batchSize) || (count > total)) {
            return false;
        }

        //  note:  "count == total" can mean done for screener, but not for lookup.   (use additional substring check to tell which one we have)
        //   TODO - make different instance to avoid this foolishness
        if (count == total && responseBodySubstring.contains("\"quotes\":[")) {
            return false;
        }

        return true;
    }
}
