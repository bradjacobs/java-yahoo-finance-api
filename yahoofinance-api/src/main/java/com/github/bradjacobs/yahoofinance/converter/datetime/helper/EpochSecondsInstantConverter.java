/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import java.time.Instant;

public class EpochSecondsInstantConverter implements EpochSecondsConverter<Instant>
{
    @Override
    public Instant convertFromEpochSeconds(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return Instant.ofEpochSecond(epochSeconds);
    }

    @Override
    public Long convertFromObject(Instant o) {
        if (o == null) {
            return null;
        }
        return o.getEpochSecond();
    }
}
