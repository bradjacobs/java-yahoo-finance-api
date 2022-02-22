package com.github.bradjacobs.yahoofinance.converter.datetime;

import org.apache.commons.lang3.StringUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class EpochSecondsConverter
{
    // if a Long value is greater than this, then assume it's in Milliseconds
    private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;

    private static final int DATE_ONLY_STRING_MAX_LENGTH = 10;  // e.g. 2021-01-31  // todo use or remove

    private static final ZoneId DEFAULT_ZONE = ZoneId.of("GMT");
    private final ZoneId zone;

    public EpochSecondsConverter() {
        this(DEFAULT_ZONE);
    }
    public EpochSecondsConverter(ZoneId zone) {
        this.zone = (zone != null ? zone : DEFAULT_ZONE);
    }

    public String toString(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        LocalDate localDate = toLocalDate(epochSeconds);
        return localDate.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    public Long fromString(String dateString) {
        if (StringUtils.isEmpty(dateString)) {
            return null;
        }
        return fromLocalDate( LocalDate.parse(dateString) );
    }

    public Instant toInstant(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return Instant.ofEpochSecond(ensureSeconds(epochSeconds));
    }

    public Long fromInstant(Instant instant) {
        if (instant == null) {
            return null;
        }
        return instant.getEpochSecond();
    }

    public LocalDate toLocalDate(Long epochSeconds) {
        if (epochSeconds == null) {
            return null;
        }
        return Instant.ofEpochSecond(ensureSeconds(epochSeconds)).atZone(zone).toLocalDate();
    }

    public Long fromLocalDate(LocalDate localDate) {
        if (localDate == null) {
            return null;
        }
        return localDate.atStartOfDay().atZone(zone).toInstant().toEpochMilli() / 1000;
    }

    public Long fromLong(Long input) {
        return ensureSeconds(input);
    }


    /**
     * If the timestamp appears "too big" then assume we are given MilliSeconds, thus convert it to seconds.
     * @param timestamp epoch timestamp
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
