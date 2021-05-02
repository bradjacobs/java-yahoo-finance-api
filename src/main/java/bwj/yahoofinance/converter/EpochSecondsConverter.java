package bwj.yahoofinance.converter;

import java.time.Instant;
import java.util.Date;

public class EpochSecondsConverter
{
    private static final EpochSecondsInstantConverter EPOCH_SECONDS_INSTANT_CONVERTER = new EpochSecondsInstantConverter();
    private static final EpochSecondsDateConverter EPOCH_SECONDS_DATE_CONVERTER = new EpochSecondsDateConverter();
    private static final EpochSecondsDateStrConverter EPOCH_SECONDS_DATE_STR_CONVERTER = new EpochSecondsDateStrConverter();


    public Instant convertToInstant(Long timestamp) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertToInstant(timestamp);
    }
    public Date convertToDate(Long timestamp) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertToDate(timestamp);
    }
    public String convertToString(Long timestamp) {
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertToString(timestamp);
    }


    public Long convertToEpochSeconds(Instant instant) {
        return EPOCH_SECONDS_INSTANT_CONVERTER.convertToEpoch(instant);
    }
    public Long convertToEpochSeconds(Date date) {
        return EPOCH_SECONDS_DATE_CONVERTER.convertToEpoch(date);
    }
    public Long convertToEpochSeconds(String dateString) {
        return EPOCH_SECONDS_DATE_STR_CONVERTER.convertToEpoch(dateString);
    }

}
