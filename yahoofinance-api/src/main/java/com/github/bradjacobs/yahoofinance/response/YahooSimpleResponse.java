package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.response.converter.YahooResponseConverter;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonConverter;

import java.util.List;
import java.util.Map;

public class YahooSimpleResponse implements YahooResponse
{
    protected final Response rawResponse;
    protected final YahooEndpoint endpoint;
    protected final YahooResponseConverter responseConverter;

    public YahooSimpleResponse(YahooEndpoint endpoint, Response rawResponse) {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an endpoint parameter.");
        }
        this.endpoint = endpoint;
        this.rawResponse = rawResponse;
        this.responseConverter = ResponseConverterFactory.getResponseConverter(endpoint);
    }

    public YahooEndpoint getEndpoint()
    {
        return endpoint;
    }

    @Override
    public int getHttpCode() {
        return this.rawResponse.getCode();
    }

    @Override
    public String getJson() {
        return this.rawResponse.getBody();
    }

    @Override
    public String getPrettyJson() {
        return JsonConverter.toPrettyJson(this.rawResponse.getBody());
    }

    @Override
    public boolean hasErrors()
    {
        return rawResponse.isError();
    }

    @Override
    public List<Map<String,Object>> getAsListOfMaps() {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }
        return responseConverter.convertToListOfMaps(rawResponse.getBody());
    }

    @Override
    public Map<String, Map<String, Object>> getAsMapOfMaps() {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }
        return responseConverter.convertToMapOfMaps(rawResponse.getBody());
    }

    @Override
    public <T> List<T> getAsListOfPojos(Class<T> targetType) {
        List<Map<String, Object>> listOfMaps = getAsListOfMaps();
        return responseConverter.convertToListOfPojos(listOfMaps, targetType);
    }

    @Override
    public <T> Map<String,T> getAsMapOfPojos(Class<T> targetType) {
        Map<String, Map<String, Object>> mapOfMaps = getAsMapOfMaps();
        return responseConverter.convertToMapOfPojos(mapOfMaps, targetType);
    }
}
