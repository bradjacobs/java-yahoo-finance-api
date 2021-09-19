package com.github.bradjacobs.yahoofinance.converter.datetime;


public interface EpochStrConverter
{
    String convertToString(Long timestamp);

    Long convertToEpoch(String str);
}