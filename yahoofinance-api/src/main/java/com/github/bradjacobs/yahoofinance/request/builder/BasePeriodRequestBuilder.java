/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;

import java.time.Instant;
import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneOffset;

/**
 * All the builder logic for setting "&period1=..."  "&period2=..." values
 *
 * DEV NOTE: this is probably overkill and when discovered that certain methods will never
 *    get used, then they will be subject to 'pruning'
 */
abstract public class BasePeriodRequestBuilder<T extends BasePeriodRequestBuilder<T>> extends BaseRequestBuilder<T>
{
    private static final ZoneOffset GMT_ZONE = ZoneOffset.UTC;
    private static final EpochSecondsConverter epochSecondsConverter = new EpochSecondsConverter(GMT_ZONE);

    protected Long startPeriod;
    protected Long endPeriod;

    protected abstract T getThis();

    public T setPeriodRange(Period period) {
        if (period != null)
        {
            this.startPeriod = LocalDate.now().minus(period).atStartOfDay(GMT_ZONE).toInstant().getEpochSecond();  // close approximate
            this.endPeriod = System.currentTimeMillis() / 1000L; // ensure include right up to 'now'
        }
        return getThis();
    }


    public T setTimeRange(String startDate, String endDate) {
        return setStart(startDate).setEnd(endDate);
    }
    public T setTimeRange(Long start, Long end) {
        return setStart(start).setEnd(end);
    }
    public T setTimeRange(Instant start, Instant end) {
        return setStart(start).setEnd(end);
    }

    // methods below as being considered for removal (or no longer public)  TBD.

    public T setStart(String startDate) {
        this.startPeriod = epochSecondsConverter.fromString(startDate);
        return getThis();
    }
    public T setStart(Instant start) {
        this.startPeriod = epochSecondsConverter.fromInstant(start);
        return getThis();
    }
    public T setStart(Long start) {
        this.startPeriod = epochSecondsConverter.fromLong(start);
        return getThis();
    }

    public T setEnd(String endDate) {
        this.endPeriod = epochSecondsConverter.fromString(endDate);
        return getThis();
    }
    public T setEnd(Instant end) {
        this.endPeriod = epochSecondsConverter.fromInstant(end);
        return getThis();
    }
    public T setEnd(Long end) {
        this.endPeriod = epochSecondsConverter.fromLong(end);
        return getThis();
    }
}
