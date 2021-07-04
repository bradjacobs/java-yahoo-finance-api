/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;

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
}
