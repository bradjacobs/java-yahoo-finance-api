package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

public interface EpochSecondsConverter<U>
{
    U convertFromEpochSeconds(final Long epochSeconds);

    Long convertFromObject(final U o);
}
