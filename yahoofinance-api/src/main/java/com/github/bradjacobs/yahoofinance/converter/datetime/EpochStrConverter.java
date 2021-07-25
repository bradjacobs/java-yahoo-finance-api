package com.github.bradjacobs.yahoofinance.converter.datetime;

import java.time.Instant;

public interface EpochStrConverter
{
    String convertToString(Long timestamp);

    Long convertToEpoch(String str);
}