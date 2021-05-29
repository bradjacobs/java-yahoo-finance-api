/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class DateStringInstantConverter
{
    private static final String DATE_PATTERN = "yyyy-MM-dd";
    private static final String TIMEZONE = "GMT";
    private static final ZoneId ZONE_ID = ZoneId.of(TIMEZONE);

    private static final DateTimeFormatter DATE_ONLY_FORMATTER =
            DateTimeFormatter.ofPattern(DATE_PATTERN).withZone(ZONE_ID);

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
        return DATE_ONLY_FORMATTER.format(instant);
    }

}
