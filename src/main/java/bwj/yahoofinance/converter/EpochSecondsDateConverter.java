package bwj.yahoofinance.converter;

import java.util.Date;

public class EpochSecondsDateConverter
{
    // todo: fix this constant
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;

    public Date convertToDate(Long timestamp) {
        if (timestamp == null) {
            return null;
        }

        if (timestamp < EPOCH_MILLI_THRESHOLD) {
            timestamp *= 1000;
        }
        return new Date(timestamp);
    }

    public Long convertToEpoch(Date date) {
        if (date == null) {
            return null;
        }
        return date.getTime() / 1000;
    }

}
