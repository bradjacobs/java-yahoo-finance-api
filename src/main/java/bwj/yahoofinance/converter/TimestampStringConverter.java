/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * Converts an epoch timestamp value (as a long) into a String format.
 *
 * For more on valid pattern values, check out docs for DateTimeFormatter
 * @see java.time.format.DateTimeFormatter
 */
public class TimestampStringConverter
{
//    private static final String DEFAULT_DATE_PATTERN = "MM/dd/yy";
//    private static final String DEFAULT_DATE_TIME_PATTERN = "MM/dd/yy hh:mm a";

    private static final String DEFAULT_DATE_PATTERN = "yyyy-MM-dd";  // appears the be the 'default' date format in Yahoo API response
    private static final String DEFAULT_DATE_TIME_PATTERN = "yyyy-MM-dd hh:mm a";

    private static final String DEFAULT_TIMEZONE = "GMT";

    private final String datePattern;
    private final String dateTimePattern;
    private final String timeZone;
    private final DateTimeFormatter dateOnlyFormatter;
    private final DateTimeFormatter dateTimeFormatter;

    // if the input epoch time is greater than this value, then assume MILLISECONDS
    //   otherwise assume SECONDS
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;


    public TimestampStringConverter()
    {
        this(DEFAULT_DATE_PATTERN, DEFAULT_DATE_TIME_PATTERN, DEFAULT_TIMEZONE);
    }

    public TimestampStringConverter(String datePattern, String dateTimePattern, String timeZone) {
        this.datePattern = datePattern;
        this.dateTimePattern = dateTimePattern;
        this.timeZone = timeZone;

        ZoneId zoneId;
        try {
            zoneId = ZoneId.of(timeZone);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid timezone: '" + timeZone + "'.", e);
        }

        try {
            dateOnlyFormatter = DateTimeFormatter.ofPattern(datePattern).withZone(zoneId);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid datePattern: '" + datePattern + "'.", e);
        }

        try {
            dateTimeFormatter = DateTimeFormatter.ofPattern(dateTimePattern).withZone(zoneId);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid dateTimePattern: '" + datePattern + "'.", e);
        }
    }

    public String toDateStr(long timestamp) {
        Instant instant = getInstant(timestamp);
        return dateOnlyFormatter.format(instant);
    }

    public String toDateTimeStr(long timestamp) {
        Instant instant = getInstant(timestamp);
        return dateTimeFormatter.format(instant);
    }

    public String getDatePattern() {
        return datePattern;
    }

    public String getDateTimePattern() {
        return dateTimePattern;
    }

    public String getTimeZone() {
        return timeZone;
    }

    private Instant getInstant(long timestamp)
    {
        if (timestamp > EPOCH_MILLI_THRESHOLD) {
            return Instant.ofEpochMilli(timestamp);
        }
        return Instant.ofEpochSecond(timestamp);
    }


    public static class Builder {
        private String datePattern = DEFAULT_DATE_PATTERN;
        private String dateTimePattern = DEFAULT_DATE_TIME_PATTERN;
        private String timeZone = DEFAULT_TIMEZONE;

        public Builder() { }

        public Builder withDatePattern(String datePattern) {
            this.datePattern = datePattern;
            return this;
        }

        public Builder withDateTimePattern(String dateTimePattern) {
            this.dateTimePattern = dateTimePattern;
            return this;
        }
        public Builder withTimeZone(String timeZone) {
            this.timeZone = timeZone;
            return this;
        }

        public TimestampStringConverter build()
        {
            return new TimestampStringConverter(datePattern, dateTimePattern, timeZone);
        }
    }
}