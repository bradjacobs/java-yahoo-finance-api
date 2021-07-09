package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.http.client.HttpResponseException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

abstract public class AbstractResponse implements YahooResult
{
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


    public List<Map<String,Object>> getAsListOfMaps() throws HttpResponseException
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

    public Map<String, Map<String, Object>> getAsMapOfMaps() throws HttpResponseException
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

}
