package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public interface YahooRequest
{
    YahooEndpoint getEndpoint();

    String getTicker();
    Map<String,String> getParamMap();

    String getPostBody();
    boolean isCrumbRequired();

    Map<String, String> getHeaderMap();
}