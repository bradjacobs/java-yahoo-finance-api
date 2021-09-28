/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import org.apache.commons.lang3.StringUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class EpochSecondsDateStringConverter implements EpochSecondsConverter<String>, EpochStrConverter
{
    private static final String DATE_PATTERN = "yyyy-MM-dd";
    private static final ZoneId ZONE_ID = ZoneId.of("GMT");

    private static final DateTimeFormatter DATE_ONLY_FORMATTER =
            DateTimeFormatter.ofPattern(DATE_PATTERN).withZone(ZONE_ID);

    @Override
    public String convertFromEpochSeconds(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return DATE_ONLY_FORMATTER.format(Instant.ofEpochSecond(epochSeconds));
    }

    @Override
    public Long convertFromObject(String o) {
        if (StringUtils.isEmpty(o)) {
            return null;
        }
        LocalDate ld = LocalDate.parse(o, DATE_ONLY_FORMATTER);
        return ld.atStartOfDay(ZONE_ID).toInstant().getEpochSecond();
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
