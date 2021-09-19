/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;

import java.time.Instant;

public class EpochSecondsDateStrConverter implements EpochStrConverter
{
    private static final EpochSecondsInstantConverter EPOCH_SECONDS_INSTANT_CONVERTER = new EpochSecondsInstantConverter();
    private static final DateStringInstantConverter DATE_STRING_INSTANT_CONVERTER = new DateStringInstantConverter();

    @Override
    public String convertToString(Long timestamp) {
        if (timestamp == null) {
            return null;
        }
        Instant instant = EPOCH_SECONDS_INSTANT_CONVERTER.convertToInstant(timestamp);
        return DATE_STRING_INSTANT_CONVERTER.convertToString(instant);
    }

    @Override
    public Long convertToEpoch(String date) {
        if (date == null) {
            return null;
        }

        Instant instant = DATE_STRING_INSTANT_CONVERTER.convertToInstant(date);
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertToEpoch(instant);
    }
}
