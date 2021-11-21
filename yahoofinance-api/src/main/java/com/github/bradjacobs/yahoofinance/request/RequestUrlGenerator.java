package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class RequestUrlGenerator
{
    private static final String BASE_API_SCHEME = "https";
    private static final String BASE_API_HOST = "query1.finance.yahoo.com";
    private static final String CRUMB_KEY = "crumb";

    public String buildRequestUrl(YahooFinanceRequest request) throws IOException
    {
        return buildRequestUrl(request, null);
    }

    public String buildRequestUrl(YahooFinanceRequest request, String crumb)
    {
        Map<String, String> paramMap = request.getParamMap();
        if (StringUtils.isNotEmpty(crumb)) {
            Map<String,String> updatedParamMap = new LinkedHashMap<>(paramMap);
            updatedParamMap.put(CRUMB_KEY, crumb);
            paramMap = updatedParamMap;
        }

        String ticker = request.getTicker().toUpperCase();
        YahooEndpoint endpoint = request.getEndpoint();

        //  e.g. /v8/finance/chart/AAPL
        String urlPath = endpoint.getPathPrefix() + "v" + endpoint.getVersion() + "/finance/" + endpoint.getName();
        if ( endpoint.isTickerOnPath() ) {
            urlPath +=  "/" + ticker;
        }

        URIBuilder builder = new URIBuilder().setScheme(BASE_API_SCHEME).setHost(BASE_API_HOST).setPath(urlPath);

        for (Map.Entry<String, String> paramEntry : paramMap.entrySet()) {
            builder.addParameter(paramEntry.getKey(), paramEntry.getValue());
        }

        return builder.toString();
    }
}
