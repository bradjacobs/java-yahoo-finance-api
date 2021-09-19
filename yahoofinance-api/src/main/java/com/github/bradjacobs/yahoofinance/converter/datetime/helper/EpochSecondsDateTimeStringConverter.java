/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import org.apache.commons.lang3.StringUtils;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class EpochSecondsDateTimeStringConverter implements EpochSecondsConverte<String>, EpochStrConverter
{
    // NOTE: no seconds b/c haven't seen any non-zero second values
    private static final String DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm";
    private static final ZoneId ZONE_ID = ZoneId.of("GMT");

    private static final DateTimeFormatter DATE_TIME_FORMATTER =
            DateTimeFormatter.ofPattern(DATE_TIME_PATTERN).withZone(ZONE_ID);

    @Override
    public String convertFromEpochSeconds(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return DATE_TIME_FORMATTER.format(Instant.ofEpochSecond(epochSeconds));
    }

    @Override
    public Long convertFromObject(String o) {
        if (StringUtils.isEmpty(o)) {
            return null;
        }
        LocalDateTime ld = LocalDateTime.parse(o, DATE_TIME_FORMATTER);
        return ld.atZone(ZONE_ID).toInstant().getEpochSecond();
    }



    @Override
    public String convertToString(Long timestamp) {
        return convertFromEpochSeconds(timestamp);
    }

    @Override
    public Long convertToEpoch(String str) {
        return convertFromObject(str);
    }
}
