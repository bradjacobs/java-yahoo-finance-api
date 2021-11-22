package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public interface YahooRequest
{
    YahooEndpoint getEndpoint();

    boolean isPost();
    String getPostBody();
    boolean isCrumbRequired();

    String getTicker();
    Map<String,String> getParamMap();


    Map<String, String> getHeaderMap();
}