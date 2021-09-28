/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import java.util.Date;

public class EpochSecondsDateConverter implements EpochSecondsConverter<Date>
{
    @Override
    public Date convertFromEpochSeconds(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return new Date(epochSeconds * 1000L);
    }

    @Override
    public Long convertFromObject(Date o) {
        if (o == null) {
            return null;
        }
        return o.getTime() / 1000L;
    }
}
