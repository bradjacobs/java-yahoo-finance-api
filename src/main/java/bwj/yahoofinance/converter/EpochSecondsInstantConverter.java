package bwj.yahoofinance.converter;

import java.time.Instant;

public class EpochSecondsInstantConverter
{
    // todo: fix this constant
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;

    public Instant convertToInstant(Long timestamp) {
        if (timestamp == null) {
            return null;
        }

        if (timestamp > EPOCH_MILLI_THRESHOLD) {
            return Instant.ofEpochMilli(timestamp);
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
