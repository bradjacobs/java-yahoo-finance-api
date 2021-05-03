/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter.datetime;

import java.time.Instant;

public class EpochSecondsInstantConverter
{
    public Instant convertToInstant(Long timestamp) {
        if (timestamp == null) {
            return null;
        }

        return Instant.ofEpochSecond(timestamp);
    }

    public Long convertToEpoch(Instant instant) {
        if (instant == null) {
            return null;
        }
        return instant.getEpochSecond();
    }
}
