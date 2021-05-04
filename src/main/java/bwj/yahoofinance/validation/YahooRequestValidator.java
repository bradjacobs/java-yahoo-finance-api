/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.validation;

import bwj.yahoofinance.enums.YahooEndpoint;
import bwj.yahoofinance.request.ParamKeys;
import bwj.yahoofinance.request.YahooFinanceRequest;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static bwj.yahoofinance.enums.YahooEndpoint.*;


public class YahooRequestValidator
{
    private static final Map<YahooEndpoint, List<String>> requiredParamsMap = new HashMap<>();

    // list of any 'required' url params for a given endpoint.
    static {
        // todo: need to look for a better home
        requiredParamsMap.put(ESG_CHART, Collections.singletonList(ParamKeys.SYMBOL));
        requiredParamsMap.put(ESG_PEER_SCORES, Collections.singletonList(ParamKeys.SYMBOL));
        requiredParamsMap.put(INSIGHTS, Collections.singletonList(ParamKeys.SYMBOL));
        requiredParamsMap.put(LOOKUP, Collections.singletonList(ParamKeys.QUERY));
        requiredParamsMap.put(LOOKUP_TOTALS, Collections.singletonList(ParamKeys.QUERY));
        requiredParamsMap.put(QUOTE, Collections.singletonList(ParamKeys.SYMBOLS));
        requiredParamsMap.put(QUOTE_SUMMARY, Collections.singletonList(ParamKeys.MODULES));
        requiredParamsMap.put(SEARCH, Collections.singletonList(ParamKeys.Q));
        requiredParamsMap.put(SPARK, Collections.singletonList(ParamKeys.SYMBOLS));
        requiredParamsMap.put(TECHNICAL_EVENTS, Collections.singletonList(ParamKeys.SYMBOL));
        requiredParamsMap.put(TIMESERIES, Arrays.asList(ParamKeys.PERIOD1, ParamKeys.PERIOD2));
        requiredParamsMap.put(VALIDATE, Collections.singletonList(ParamKeys.SYMBOLS));
    }


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
        if (paramMap == null) {
            paramMap = Collections.emptyMap();
        }

        if (!paramMap.isEmpty())
        {
            for (Map.Entry<String, String> entry : paramMap.entrySet()) {
                if (StringUtils.isEmpty(entry.getKey())) {
                    throw new IllegalArgumentException("Cannot have a blank parameter key");
                }

                // still TBD if blank values should ever be allowed empty or not.
            }
        }

        List<String> requiredParams = requiredParamsMap.get(endpoint);
        if (requiredParams != null)
        {
            for (String requiredParam : requiredParams) {
                String paramValue = paramMap.get(requiredParam);
                throw new IllegalArgumentException(String.format("Endpoint %s is missing required parameter '%s'.", endpoint, requiredParam));
            }
        }
    }
}
