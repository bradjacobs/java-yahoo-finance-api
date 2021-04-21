/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import org.apache.commons.lang.StringUtils;

import java.util.Map;
import java.util.Set;

public class YahooRequestValidator
{
    public void validationRequest(YahooFinanceRequest request)
    {
        if (request == null) {
            throw new IllegalArgumentException("Request cannot be null.");
        }

        if (StringUtils.isEmpty(request.getTicker())) {
            throw new IllegalArgumentException("Request is missing a valid ticker value.");
        }

        YahooEndpoint endpoint = request.getEndpoint();
        if (endpoint == null) {
            throw new IllegalArgumentException("Request is missing endpoint value.");
        }

        // if there are multiple endpoints, they must all match the first.
        Set<YahooEndpoint> endpoints = request.getEndpoints();
        if (endpoints != null && endpoints.size() > 1) {

            for (YahooEndpoint yahooEndpoint : endpoints) {
                if (!yahooEndpoint.isQuoteSummaryModule()) {
                    throw new IllegalArgumentException("Multiple endpoints are only allowed for quote summary requests.");
                }
                if (yahooEndpoint.getVersion() != endpoint.getVersion()) {
                    throw new IllegalArgumentException("At least 2 endpoints have different versions.");
                }
            }
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
    }
}
