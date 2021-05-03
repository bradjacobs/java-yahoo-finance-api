/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.validation;

import bwj.yahoofinance.enums.YahooEndpoint;
import bwj.yahoofinance.request.YahooFinanceRequest;
import org.apache.commons.lang.StringUtils;

import java.util.Map;

public class YahooRequestValidator
{
    public void validationRequest(YahooFinanceRequest request)
    {
        if (request == null) {
            throw new IllegalArgumentException("Request cannot be null.");
        }

        YahooEndpoint endpoint = request.getEndpoint();
        if (endpoint == null) {
            throw new IllegalArgumentException("Request is missing endpoint value.");
        }

        if (StringUtils.isEmpty(request.getTicker()) && !endpoint.getIsQuery()) {
            throw new IllegalArgumentException("Request is missing a valid ticker value.");
        }


        Map<String, String> paramMap = request.getParamMap();
        if (paramMap != null && !paramMap.isEmpty())
        {
            for (Map.Entry<String, String> entry : paramMap.entrySet()) {
                if (StringUtils.isEmpty(entry.getKey())) {
                    throw new IllegalArgumentException("Cannot have a blank parameter key");
                }

                // still TBD if blank values should ever be allowed empty or not.
            }
        }

        if (endpoint.equals(YahooEndpoint.QUOTE_SUMMARY))
        {
            String modulesValue = null;
            if (paramMap != null) {
                modulesValue = paramMap.get("modules");
            }

            if (StringUtils.isEmpty(modulesValue)) {
                // this particular case want to catch before even trying to make Http request.
                throw new IllegalArgumentException("QuoteSummary endpoint must have 1 or more modules value.");
            }
        }
    }
}
