package com.github.bradjacobs.yahoofinance.validation;

import com.github.bradjacobs.yahoofinance.YahooFinanceClient;
import com.github.bradjacobs.yahoofinance.converter.json.ChartDataConverter;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.lang3.NotImplementedException;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

// todo - try to think of better name.
public class YahooFinanceMapClient
{
    // this is in-flux, so temporarily keep a list of supported endpoints (and return error if another type is detected)
    private static final Set<YahooEndpoint> SUPPORTED_ENDPOINTS =
        new HashSet<>(Arrays.asList(YahooEndpoint.SCREENER, YahooEndpoint.QUOTE, YahooEndpoint.CHART));

    private static final ChartDataConverter CHART_DATA_CONVERTER = new ChartDataConverter();

    private final YahooFinanceClient yahooClient;

    public YahooFinanceMapClient(YahooFinanceClient yahooClient) {
        this.yahooClient = yahooClient;
    }

    public void setContentType(String contentType) {
        yahooClient.setContentType(contentType);
    }

    public void setUserAgent(String userAgent) {
        yahooClient.setUserAgent(userAgent);
    }

    public List<Map<String,Object>> executeRequest(YahooFinanceRequest request) throws IOException
    {
        String json = yahooClient.executeRequest(request);
        return convertResponse(request.getEndpoint(), json);
    }



    //   side tangent:   still trying to understand why some folks like this syntax for this scenario
    //private static final Set<YahooEndpoint> SUPPORTED_ENDPOINTS = Stream.of(YahooEndpoint.SCREENER, YahooEndpoint.CHART).collect(Collectors.toCollection(HashSet::new));

    protected List<Map<String,Object>> convertResponse(YahooEndpoint endpoint, String jsonResponse)
    {
        if (! SUPPORTED_ENDPOINTS.contains(endpoint)) {
            throw new NotImplementedException("Method is not yet supported for endpoint: " + endpoint);
        }

        // TODO TODO TODO
        //   - will swing back around to clean this up.
        //    for now just want to see it work

        if (YahooEndpoint.SCREENER.equals(endpoint)) {
            return JsonPath.read(jsonResponse, "$.finance.result[0].quotes");
        }
        else if (YahooEndpoint.QUOTE.equals(endpoint)) {
            return JsonPath.read(jsonResponse, "$.quoteResponse.result");
        }
        else if (YahooEndpoint.CHART.equals(endpoint)) {
            return CHART_DATA_CONVERTER.toListOfMaps2(jsonResponse);
        }
        else {
            throw new IllegalArgumentException("Invalid endpoint: " + endpoint);
        }
    }

}
