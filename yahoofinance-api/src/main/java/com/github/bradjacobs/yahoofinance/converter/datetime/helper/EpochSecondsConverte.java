package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

public interface EpochSecondsConverte<U>
{
    U convertFromEpochSeconds(final Long epochSeconds);

    Long convertFromObject(final U o);
}
