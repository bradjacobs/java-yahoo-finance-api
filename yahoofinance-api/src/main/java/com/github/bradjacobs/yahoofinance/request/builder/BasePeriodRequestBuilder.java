/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;

/**
 * All the builder logic for setting "&period1=..."  "&period2=..." values
 *
 * DEV NOTE: this is probably overkill and when discovered that certain methods will never
 *    get used, then they will be subject to 'pruning'
 */
abstract public class BasePeriodRequestBuilder<T extends BasePeriodRequestBuilder<T>> extends BaseRequestBuilder<T>
{
    private static final MetaEpochSecondsConverter epochSecondsConverter = MetaEpochSecondsConverter.getInstance();
    private static final ZoneId GMT_ZONE = ZoneId.of("GMT");

    protected Long startPeriod;
    protected Long endPeriod;

    protected abstract T getThis();

    public T setTimeRangeLastXHours(int hours) {
        return setTimeRangeLastXUnits(hours, ChronoUnit.HOURS);
    }
    public T setTimeRangeLastXDays(int days) {
        return setTimeRangeLastXUnits(days, ChronoUnit.DAYS);
    }
    public T setTimeRangeLastXWeeks(int weeks) {
        return setTimeRangeLastXUnits(weeks, ChronoUnit.WEEKS);
    }
    public T setTimeRangeLastXMonths(int months) {
        return setTimeRangeLastXUnits(months, ChronoUnit.MONTHS);
    }
    public T setTimeRangeLastXYears(int years) {
        return setTimeRangeLastXUnits(years, ChronoUnit.YEARS);
    }

    public T setTimeRange(String startDate, String endDate) {
        return setStart(startDate).setEnd(endDate);
    }
    public T setTimeRange(Long start, Long end) {
        return setStart(start).setEnd(end);
    }
    public T setTimeRange(Date start, Date end) {
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
    public T setStart(Date start) {
        this.startPeriod = epochSecondsConverter.fromDate(start);
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
    public T setEnd(Date end) {
        this.endPeriod = epochSecondsConverter.fromDate(end);
        return getThis();
    }
    public T setEnd(Long end) {
        this.endPeriod = epochSecondsConverter.fromLong(end);
        return getThis();
    }

    protected T setTimeRangeLastXUnits(int value, ChronoUnit unit) {
        Instant instantNow = Instant.now();
        LocalDateTime ldt = LocalDateTime.from(instantNow.atZone(GMT_ZONE));

        long endPeriodSeconds = instantNow.getEpochSecond();
        long startPeriodSeconds = 0;

        // Note: would be simpler to always do:
        //     Instant instantStart = instantNow.minus(value, unit);
        // HOWEVER - Instant doesn't support 'bigger' units (WEEKS,MONTHS,YEARS) for this operation.
        //   "Period" is typically for bigger units, "Duration" typically for smaller units.

        switch (unit) {
            case HOURS:
            case DAYS:
                startPeriodSeconds = instantNow.minus(value, unit).getEpochSecond();
                break;
            case WEEKS:
                startPeriodSeconds = ldt.minusWeeks(value).atZone(GMT_ZONE).toInstant().getEpochSecond();
                break;
            case MONTHS:
                startPeriodSeconds = ldt.minusMonths(value).atZone(GMT_ZONE).toInstant().getEpochSecond();
                break;
            case YEARS:
                startPeriodSeconds = ldt.minusYears(value).atZone(GMT_ZONE).toInstant().getEpochSecond();
                break;
            default:
                throw new IllegalArgumentException("Unsupported ChronoUnit: " + unit);
        }

        this.startPeriod = startPeriodSeconds;
        this.endPeriod = endPeriodSeconds;

        return getThis();
    }
}
