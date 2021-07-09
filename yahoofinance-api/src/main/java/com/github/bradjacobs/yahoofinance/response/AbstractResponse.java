package com.github.bradjacobs.yahoofinance.response;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.request.builder.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.http.client.HttpResponseException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

abstract public class AbstractResponse implements YahooResult
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();


    protected final YahooEndpoint endpoint;
    protected final YahooResponseConverter responseConverter;


    public AbstractResponse(YahooEndpoint endpoint)
    {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an endpoint parameter.");
        }
        this.endpoint = endpoint;
        this.responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);;
    }

    abstract protected List<Response> getListResponse();


    public YahooEndpoint getEndpoint()
    {
        return endpoint;
    }

    public YahooResponseConverter getResponseConverter()
    {
        return responseConverter;
    }


    public List<Map<String,Object>> getAsListOfMaps()
    {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }

        List<Response> listResponse = getListResponse();
        if (listResponse == null || listResponse.isEmpty()) {
            return Collections.emptyList();
        }

        if (listResponse.size() == 1) {
            return responseConverter.convertToListOfMaps(listResponse.get(0).getBody());
        }
        else {
            List<Map<String,Object>> totalResults = new ArrayList<>();
            for (Response response : listResponse) {
                totalResults.addAll(responseConverter.convertToListOfMaps(response.getBody()));
            }
            return totalResults;
        }
    }

    public Map<String, Map<String, Object>> getAsMapOfMaps()
    {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }

        List<Response> listResponse = getListResponse();
        if (listResponse == null || listResponse.isEmpty()) {
            return Collections.emptyMap();
        }

        if (listResponse.size() == 1) {
            return responseConverter.convertToMapOfMaps(listResponse.get(0).getBody());
        }
        else {
            Map<String, Map<String, Object>> totalResults = new LinkedHashMap<>(); // todo - which map type to use
            for (Response response : listResponse) {
                totalResults.putAll(responseConverter.convertToMapOfMaps(response.getBody()));
            }
            return totalResults;
        }
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> getAsListOfPojos(Class<T> targetType)
    {
        validateTargetClass(targetType);

        List<Map<String, Object>> listOfMaps = getAsListOfMaps();
        if (listOfMaps.isEmpty()) {
            return Collections.emptyList();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(List.class, targetType);
        return mapper.convertValue(listOfMaps, javaType);
    }

    @SuppressWarnings("unchecked")
    public <T> Map<String,T> getAsMapOfPojos(Class<T> targetType)
    {
        validateTargetClass(targetType);

        Map<String, Map<String, Object>> mapOfMaps = getAsMapOfMaps();
        if (mapOfMaps.isEmpty()) {
            return Collections.emptyMap();
        }

        JavaType javaType = mapper.getTypeFactory().constructParametricType(Map.class, String.class, targetType);
        return mapper.convertValue(mapOfMaps, javaType);
    }

    private <T> void validateTargetClass(Class<T> targetType)
    {
        // just null check (for now)
        if (targetType == null) {
            throw new IllegalArgumentException("Must provide a target class type.");
        }
    }


}
