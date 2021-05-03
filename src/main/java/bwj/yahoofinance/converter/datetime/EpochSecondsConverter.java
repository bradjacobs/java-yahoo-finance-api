/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter.datetime;

import java.time.Instant;
import java.util.Date;

/**
 * Uber-converter for multiple types to be converted to/from unix-epoch-seconds.
 */
public class EpochSecondsConverter
{
    private static final EpochSecondsInstantConverter EPOCH_SECONDS_INSTANT_CONVERTER = new EpochSecondsInstantConverter();
    private static final EpochSecondsDateConverter EPOCH_SECONDS_DATE_CONVERTER = new EpochSecondsDateConverter();
    private static final EpochSecondsDateStrConverter EPOCH_SECONDS_DATE_STR_CONVERTER = new EpochSecondsDateStrConverter();

    // if a Long value is greater than this, then assume it's in Milliseconds
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;


    /**
     * Convert instant to Unix time seconds
     * @param instant
     * @return
     */
    public Long convertToEpochSeconds(Instant instant) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertToEpoch(instant);
    }

    /**
     * Convert date to Unix time seconds
     * @param date
     * @return
     */
    public Long convertToEpochSeconds(Date date) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertToEpoch(date);
    }

    /**
     * Convert a date string to unix time seconds.
     *   NOTE: dateString _MUST_ have the form of "yyyy-MM-dd"
     * @param dateString
     * @return
     */
    public Long convertToEpochSeconds(String dateString) {
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertToEpoch(dateString);
    }

    /**
     * this is a no-op  _UNLESS_ milliseconds is detected, then it will convert to seconds
     * @param timestamp
     * @return
     */
    public Long convertToEpochSeconds(Long timestamp) {
        return ensureSeconds(timestamp);
    }



    public Instant convertToInstant(Long timestamp) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertToInstant(ensureSeconds(timestamp));
    }
    public Date convertToDate(Long timestamp) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertToDate(ensureSeconds(timestamp));
    }
    public String convertToString(Long timestamp) {
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertToString(ensureSeconds(timestamp));
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
}
