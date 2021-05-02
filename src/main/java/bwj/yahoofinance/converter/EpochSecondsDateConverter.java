/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter;

import java.util.Date;

public class EpochSecondsDateConverter
{

    public Date convertToDate(Long timestamp) {
        if (timestamp == null) {
            return null;
        }

        timestamp *= 1000L;
        return new Date(timestamp);
    }

    public Long convertToEpoch(Date date) {
        if (date == null) {
            return null;
        }
        return date.getTime() / 1000L;
    }

}
