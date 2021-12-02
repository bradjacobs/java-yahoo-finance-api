package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonConverter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

// TODO -- need to respect the 'maxResults' value when converting to list or maps
public class YahooCompositeResponse implements YahooResponse
{
    protected final List<YahooResponse> children = new ArrayList<>();
    private final int maxResults;

    public YahooCompositeResponse(YahooEndpoint endpoint, List<Response> responseList, int maxResults) {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an endpoint parameter.");
        }
        if (responseList == null || responseList.isEmpty()) {
            throw new IllegalArgumentException("Must provide non-empty responseList.");
        }
        for (Response response : responseList) {
            children.add(new YahooSimpleResponse(endpoint, response));
        }
        this.maxResults = maxResults;
    }

    @Override
    public int getHttpCode() {
        int httpCode = 0;
        for (YahooResponse child : children) {
            httpCode = Math.max(httpCode, child.getHttpCode());
        }
        return httpCode;
    }

    @Override
    public boolean hasErrors() {
        for (YahooResponse child : children) {
            if (child.hasErrors()) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String getJson() {
        return createCombinedJson(false);
    }

    @Override
    public String getPrettyJson() {
        return createCombinedJson(true);
    }

    @Override
    public List<Map<String,Object>> getAsListOfMaps() {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }

        List<Map<String,Object>> totalResults = new ArrayList<>();
        for (YahooResponse response : children) {
            totalResults.addAll(response.getAsListOfMaps());
        }
        return totalResults;
    }

    @Override
    public Map<String, Map<String, Object>> getAsMapOfMaps() {
        if (this.hasErrors()) {
            throw new IllegalStateException("Unable to convert response to list: response has errors");
        }

        Map<String, Map<String, Object>> totalResults = new LinkedHashMap<>();
        for (YahooResponse response : children) {
            totalResults.putAll(response.getAsMapOfMaps());
        }
        return totalResults;
    }

    @Override
    public <T> List<T> getAsListOfPojos(Class<T> targetType) {
        List<T> resultList = new ArrayList<>();
        for (YahooResponse child : children) {
            resultList.addAll(child.getAsListOfPojos(targetType));
        }
        return resultList;
    }

    @Override
    public <T> Map<String,T> getAsMapOfPojos(Class<T> targetType) {
        Map<String,T> resultMap = new TreeMap<>();  // todo - confirm if always want sorted
        for (YahooResponse child : children) {
            resultMap.putAll(child.getAsMapOfPojos(targetType));
        }
        return resultMap;
    }

    private String createCombinedJson(boolean makePretty) {
        if (children.size() == 1) {
            return (makePretty ? children.get(0).getPrettyJson() : children.get(0).getJson());
        }

        List<String> jsonList = new ArrayList<>();
        for (YahooResponse child : children) {
            jsonList.add(child.getJson());
        }
        String combinedJson = Arrays.toString(jsonList.toArray(new String[0]));
        if (makePretty) {
            combinedJson = JsonConverter.toPrettyJson(combinedJson);
        }
        return combinedJson;
    }
}
