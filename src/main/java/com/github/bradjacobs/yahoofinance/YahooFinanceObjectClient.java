package com.github.bradjacobs.yahoofinance;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.NotImplementedException;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

// todo - try to think of better name.
public class YahooFinanceObjectClient
{
    private static final JsonMapper mapper = new JsonMapper();

    // this is in-flux, so temporarily keep a list of supported endpoints (and return error if another type is detected)
    private static final Set<YahooEndpoint> SUPPORTED_ENDPOINTS =
        new HashSet<>(Arrays.asList(YahooEndpoint.SCREENER, YahooEndpoint.TIMESERIES, YahooEndpoint.CHART));

    private final YahooFinanceClient yahooClient;

    public YahooFinanceObjectClient(YahooFinanceClient yahooClient) {
        this.yahooClient = yahooClient;
    }

    public void setContentType(String contentType) {
        yahooClient.setContentType(contentType);
    }

    public void setUserAgent(String userAgent) {
        yahooClient.setUserAgent(userAgent);
    }


    @SuppressWarnings("unchecked")
    public <T> List<T> fetchObjects(YahooFinanceRequest request, Class<T> targetType) throws IOException
    {
        validateSupportedEndpoint(request);
        List<Map<String,Object>> listOfMaps = yahooClient.executeListRequest(request);
        JavaType javaType = mapper.getTypeFactory().constructParametricType(List.class, targetType);
        List<T> resultList = mapper.convertValue(listOfMaps, javaType);
        return resultList;
    }

    @SuppressWarnings("unchecked")
    public <T> Map<String,T> fetchMappedObjects(YahooFinanceRequest request, Class<T> targetType) throws IOException
    {
        validateSupportedEndpoint(request);
        Map<String, Map<String, Object>> mapOfMaps = yahooClient.executeMapRequest(request);
        JavaType javaType = mapper.getTypeFactory().constructParametricType(Map.class, String.class, targetType);
        Map<String,T> resultMap = mapper.convertValue(mapOfMaps, javaType);
        return resultMap;
    }



    private void validateSupportedEndpoint(YahooFinanceRequest request)
    {
        if (request != null)
        {
            YahooEndpoint endpoint = request.getEndpoint();
            if (endpoint != null && ! SUPPORTED_ENDPOINTS.contains(endpoint)) {
                throw new NotImplementedException("Method is not yet supported for endpoint: " + endpoint);
            }
        }
    }


}
