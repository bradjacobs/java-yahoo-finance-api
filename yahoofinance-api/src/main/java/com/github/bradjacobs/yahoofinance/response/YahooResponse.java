package com.github.bradjacobs.yahoofinance.response;

import java.util.List;
import java.util.Map;

// todo - add javadocs
public interface YahooResponse
{
    int getHttpCode();
    boolean hasErrors();

    String getJson();

    String getPrettyJson();

    List<Map<String,Object>> getAsListOfMaps();
    Map<String, Map<String, Object>> getAsMapOfMaps();
    <T> List<T> getAsListOfPojos(Class<T> targetType);
    <T> Map<String,T> getAsMapOfPojos(Class<T> targetType);
}
