/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsDateConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsDateStringConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsDateTimeStringConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsInstantConverter;

import java.time.Instant;
import java.util.Date;

/**
 * Uber-converter for multiple types to be converted to/from unix-epoch-seconds.
 */
public class MetaEpochSecondsConverter
{
    private static final EpochSecondsInstantConverter EPOCH_SECONDS_INSTANT_CONVERTER = new EpochSecondsInstantConverter();
    private static final EpochSecondsDateConverter EPOCH_SECONDS_DATE_CONVERTER = new EpochSecondsDateConverter();
    private static final EpochSecondsDateStringConverter EPOCH_SECONDS_DATE_STR_CONVERTER = new EpochSecondsDateStringConverter();
    private static final EpochSecondsDateTimeStringConverter EPOCH_SECONDS_DATE_TIME_STR_CONVERTER = new EpochSecondsDateTimeStringConverter();

    // if a Long value is greater than this, then assume it's in Milliseconds
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;
    private static final int DATE_ONLY_STRING_MAX_LENGTH = 10;  // e.g. 2021-01-31

    private static final MetaEpochSecondsConverter instance = new MetaEpochSecondsConverter();

    public static MetaEpochSecondsConverter getInstance() {
        return instance;
    }

    private MetaEpochSecondsConverter() { }

    public static EpochStrConverter getDateStringConverter() {
        return EPOCH_SECONDS_DATE_STR_CONVERTER;
    }
    public static EpochStrConverter getDateTimeStringConverter() {
        return EPOCH_SECONDS_DATE_TIME_STR_CONVERTER;
    }

    /**
     * Convert Instant to Epoch Seconds
     * @param instant instant
     * @return epoch seconds
     */
    public Long fromInstant(Instant instant) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertFromObject(instant);
    }

    /**
     * Convert date to Epoch Seconds
     * @param date date
     * @return epoch seconds
     */
    public Long fromDate(Date date) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertFromObject(date);
    }


    public Long fromLong(Long input) {
        return ensureSeconds(input);
    }


    /**
     * Convert String to Epoch Seconds
     *   (limited to basic date or datetime string format)
     * @param dateString dateString
     * @return epoch seconds
     */
    public Long fromString(String dateString) {
        if (dateString != null && dateString.trim().length() > DATE_ONLY_STRING_MAX_LENGTH) {
            return EPOCH_SECONDS_DATE_TIME_STR_CONVERTER.convertFromObject(dateString);
        }
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertFromObject(dateString);
    }

    public Instant toInstant(Long epochSeconds) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertFromEpochSeconds(ensureSeconds(epochSeconds));
    }
    public Date toDate(Long epochSeconds) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertFromEpochSeconds(ensureSeconds(epochSeconds));
    }
    public String toDateString(Long epochSeconds) {
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertFromEpochSeconds(ensureSeconds(epochSeconds));
    }
    public String toDateTimeString(Long epochSeconds) {
        return EPOCH_SECONDS_DATE_TIME_STR_CONVERTER.convertFromEpochSeconds(ensureSeconds(epochSeconds));
    }

    /**
     * If the timestamp appears "too big" then assume we are given MilliSeconds, and convert it to seconds.
     * @param timestamp
     * @return
     */
    private Long ensureSeconds(Long timestamp)
    {
        if (timestamp != null && timestamp > EPOCH_MILLI_THRESHOLD) {
            timestamp /= 1000;
        }
        return timestamp;
    }


    // if 2 adjacent timestamps are within this interval threshold, then consider it 'small interval'
    //   and use 'datetime' instead of 'date' for string representation.
    private static final long SMALL_TIMESTAMP_INTERVAL_SECONDS = 60 * 60 * 23; // (23 hours in seconds)


    public static EpochStrConverter selectDateStrConverter(Long[] timestampValues)
    {
        return selectDateStrConverter(timestampValues, true);
    }

    public static EpochStrConverter selectDateStrConverter(Long[] timestampValues, boolean autoDetect)
    {
        if (autoDetect)
        {
            if (timestampValues != null && timestampValues.length > 1)
            {
                Long timestamp1 = timestampValues[0];
                Long timestamp2 = timestampValues[1];
                if (timestamp1 != null && timestamp2 != null)
                {
                    if (Math.abs(timestamp1 - timestamp2) < SMALL_TIMESTAMP_INTERVAL_SECONDS) {
                        return MetaEpochSecondsConverter.getDateTimeStringConverter();
                    }
                }
            }
        }

        return MetaEpochSecondsConverter.getDateStringConverter();
    }
}
