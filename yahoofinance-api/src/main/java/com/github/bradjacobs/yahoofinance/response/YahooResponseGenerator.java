package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;

import java.util.List;

// todo - the original need for this class is no longer, thus this class might get removed.
public class YahooResponseGenerator
{

    public YahooResponse makeResposne(YahooFinanceRequest request, Response rawResponse)
    {
        return new YahooResponse(request.getEndpoint(), rawResponse);
    }

    public YahooBatchResponse makeBatchResponse(YahooFinanceBatchRequest request, List<Response> rawResponses)
    {
        return new YahooBatchResponse(request.getEndpoint(), rawResponses);
    }

}
