/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request.builder;

import bwj.yahoofinance.converter.datetime.EpochSecondsConverter;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;

/**
 * All the builder logic for setting "&period1=..."  "&period2=..." values
 *
 * DEV NOTE: this is probably overkill and when discovered that certain methods will never
 *    get used, then they will be subject to 'pruning'
 */
abstract public class PeriodRequestParamBuilder<T extends PeriodRequestParamBuilder<T>> extends BaseRequestParamMapBuilder<T>
{
    private static final EpochSecondsConverter epochSecondsConverter = new EpochSecondsConverter();
    private static final ZoneId GMT_ZONE = ZoneId.of("GMT");

    protected Long startPeriod;
    protected Long endPeriod;

    protected abstract T getThis();

    public T withLastXHours(int hours) {
        return withLastXUnits(hours, ChronoUnit.HOURS);
    }
    public T withLastXDays(int days) {
        return withLastXUnits(days, ChronoUnit.DAYS);
    }
    public T withLastXWeeks(int weeks) {
        return withLastXUnits(weeks, ChronoUnit.WEEKS);
    }
    public T withLastXMonths(int months) {
        return withLastXUnits(months, ChronoUnit.MONTHS);
    }
    public T withLastXYears(int years) {
        return withLastXUnits(years, ChronoUnit.YEARS);
    }


    public T withTimeRange(String startDate, String endDate) {
        return withStart(startDate).withEnd(endDate);
    }
    public T withTimeRange(Long start, Long end) {
        return withStart(start).withEnd(end);
    }
    public T withTimeRange(Date start, Date end) {
        return withStart(start).withEnd(end);
    }
    public T withTimeRange(Instant start, Instant end) {
        return withStart(start).withEnd(end);
    }

    // methods below as being considered for removal (or no longer public)  TBD.

    public T withStart(String startDate) {
        this.startPeriod = epochSecondsConverter.convertToEpochSeconds(startDate);
        return getThis();
    }
    public T withStart(Instant start) {
        this.startPeriod = epochSecondsConverter.convertToEpochSeconds(start);
        return getThis();
    }
    public T withStart(Date start) {
        this.startPeriod = epochSecondsConverter.convertToEpochSeconds(start);
        return getThis();
    }
    public T withStart(Long start) {
        this.startPeriod = epochSecondsConverter.convertToEpochSeconds(start);
        return getThis();
    }

    public T withEnd(String endDate) {
        this.endPeriod = epochSecondsConverter.convertToEpochSeconds(endDate);
        return getThis();
    }
    public T withEnd(Instant end) {
        this.endPeriod = epochSecondsConverter.convertToEpochSeconds(end);
        return getThis();
    }
    public T withEnd(Date end) {
        this.endPeriod = epochSecondsConverter.convertToEpochSeconds(end);
        return getThis();
    }
    public T withEnd(Long end) {
        this.endPeriod = epochSecondsConverter.convertToEpochSeconds(end);
        return getThis();
    }


    protected T withLastXUnits(int value, ChronoUnit unit) {
        Instant instantNow = Instant.now();
        Instant instantStart = null;

        // note: would be simpler to always just do this:
        //     Instant instantStart = instantNow.minus(value, unit);
        // HOWEVER.. Instant doesn't support 'bigger' units (WEEKS,MONTHS,YEARS) for this operation.

        LocalDateTime ldt = LocalDateTime.from(instantNow.atZone(GMT_ZONE));
        switch (unit) {
            case HOURS:
            case DAYS:
                instantStart = instantNow.minus(value, unit);
                break;
            case WEEKS:
                //instantStart = instantNow.minus(value*7, ChronoUnit.DAYS);
                ldt = ldt.minusWeeks(value);
                instantStart = ldt.atZone(GMT_ZONE).toInstant();
                break;
            case MONTHS:
                ldt = ldt.minusMonths(value);
                instantStart = ldt.atZone(GMT_ZONE).toInstant();
                break;
            case YEARS:
                ldt = ldt.minusYears(value);
                instantStart = ldt.atZone(GMT_ZONE).toInstant();
                break;
            default:
                throw new IllegalArgumentException("Unsupported ChronoUnit: " + unit);
        }

        this.startPeriod = instantStart.getEpochSecond();
        this.endPeriod = instantNow.getEpochSecond();
        return getThis();
    }

}
