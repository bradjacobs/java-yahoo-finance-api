/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class DateTimeStringInstantConverter
{
    private static final String DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm";  // note: no seconds b/c haven't seen any non-zero second values
    private static final ZoneId ZONE_ID = ZoneId.of("GMT");

    private static final DateTimeFormatter DATE_TIME_FORMATTER =
            DateTimeFormatter.ofPattern(DATE_TIME_PATTERN).withZone(ZONE_ID);

    public Instant convertToInstant(String date) {
        if (date == null) {
            return null;
        }

        LocalDate ld = LocalDate.parse(date);
        return ld.atStartOfDay(ZONE_ID).toInstant();
    }

    public String convertToString(Instant instant) {
        if (instant == null) {
            return null;
        }
        return DATE_TIME_FORMATTER.format(instant);
    }
}
