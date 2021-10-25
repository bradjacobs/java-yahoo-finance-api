/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;

import java.util.Objects;

/**
 * Represents a unique record entry from a CHART response
 *
 *   IMPORTANT NOTE:  playing around to see how this might look,
 *   may ultimately decide this class isn't worth having and remove this it.
 */
public class ChartResult
{
    private String date;
    private Long timestamp;
    private Double open;
    private Double close;
    private Double adjclose;
    private Double low;
    private Double high;
    private Long volume;

    public String getDate()
    {
        return date;
    }

    public Long getTimestamp()
    {
        return timestamp;
    }

    public Double getOpen()
    {
        return open;
    }

    public Double getClose()
    {
        return close;
    }

    public Double getAdjclose()
    {
        return adjclose;
    }

    public Double getLow()
    {
        return low;
    }

    public Double getHigh()
    {
        return high;
    }

    public Long getVolume()
    {
        return volume;
    }

    public void setDate(String date)
    {
        this.date = date;
    }

    public void setTimestamp(Long timestamp)
    {
        this.timestamp = timestamp;
    }

    public void setOpen(Double open)
    {
        this.open = open;
    }

    public void setClose(Double close)
    {
        this.close = close;
    }

    public void setAdjclose(Double adjclose)
    {
        this.adjclose = adjclose;
    }

    public void setLow(Double low)
    {
        this.low = low;
    }

    public void setHigh(Double high)
    {
        this.high = high;
    }

    public void setVolume(Long volume)
    {
        this.volume = volume;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ChartResult that = (ChartResult) o;
        return Objects.equals(date, that.date) && Objects.equals(timestamp, that.timestamp) && Objects.equals(open, that.open) && Objects.equals(close, that.close) && Objects.equals(adjclose, that.adjclose) && Objects.equals(low, that.low) && Objects.equals(high, that.high) && Objects.equals(volume, that.volume);
    }

    @Override
    public int hashCode() {
        return Objects.hash(date, timestamp, open, close, adjclose, low, high, volume);
    }
}
