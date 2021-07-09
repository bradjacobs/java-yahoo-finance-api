package com.github.bradjacobs.yahoofinance.response;

public interface YahooResult<T>
{
    T getJson();
    boolean hasErrors();
}
