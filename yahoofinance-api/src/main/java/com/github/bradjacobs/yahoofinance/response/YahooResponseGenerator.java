package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.ParamKeys;

import java.util.List;
import java.util.Map;

public class YahooResponseGenerator
{
    private static final ResponseConverterConfig DATETIME_CONFIG = new ResponseConverterConfig(true, true);



    public YahooResponse makeResposne(YahooFinanceRequest request, Response rawResponse)
    {
        if (containsSmallTimeIntervalParameter(request)) {
            return new YahooResponse(request.getEndpoint(), DATETIME_CONFIG, rawResponse);
        }

        return new YahooResponse(request.getEndpoint(), rawResponse);
    }

    public YahooBatchResponse makeBatchResponse(YahooFinanceBatchRequest request, List<Response> rawResponses)
    {
        if (containsSmallTimeIntervalParameter(request)) {
            return new YahooBatchResponse(request.getEndpoint(), DATETIME_CONFIG, rawResponses);
        }

        return new YahooBatchResponse(request.getEndpoint(), rawResponses);
    }


    private boolean containsSmallTimeIntervalParameter(YahooFinanceRequest request)
    {
        Map<String, String> paramMap = request.getParamMap();
        if (paramMap != null) {
            String intervalValue = paramMap.get(ParamKeys.INTERVAL);
            if (intervalValue != null)
            {
                return intervalValue.endsWith("m") || intervalValue.endsWith("h");
            }
        }
        return false;
    }

}
