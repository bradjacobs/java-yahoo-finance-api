/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.Objects;

/**
 * Represents a unique record entry from a CHART response
 */
@JsonPropertyOrder( {
        "date",
        "open",
        "high",
        "low",
        "close",
        "adjclose",
        "volume",
})
public class ChartResult
{
    @JsonProperty("date")
    @JsonAlias("Date")
    private String date;

    @JsonProperty("open")
    @JsonAlias("Open")
    private Double open;

    @JsonProperty("high")
    @JsonAlias("High")
    private Double high;

    @JsonProperty("low")
    @JsonAlias("Low")
    private Double low;

    @JsonProperty("close")
    @JsonAlias("Close")
    private Double close;

    @JsonProperty("adjclose")
    @JsonAlias("Adj Close")
    private Double adjclose;

    @JsonProperty("volume")
    @JsonAlias("Volume")
    private Long volume;

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public Double getOpen() {
        return open;
    }

    public void setOpen(Double open) {
        this.open = open;
    }

    public Double getHigh() {
        return high;
    }

    public void setHigh(Double high) {
        this.high = high;
    }

    public Double getLow() {
        return low;
    }

    public void setLow(Double low) {
        this.low = low;
    }

    public Double getClose() {
        return close;
    }

    public void setClose(Double close) {
        this.close = close;
    }

    public Double getAdjclose() {
        return adjclose;
    }

    public void setAdjclose(Double adjclose) {
        this.adjclose = adjclose;
    }

    public Long getVolume() {
        return volume;
    }

    public void setVolume(Long volume) {
        this.volume = volume;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ChartResult that = (ChartResult) o;
        return Objects.equals(date, that.date) && Objects.equals(open, that.open) && Objects.equals(close, that.close) && Objects.equals(adjclose, that.adjclose) && Objects.equals(low, that.low) && Objects.equals(high, that.high) && Objects.equals(volume, that.volume);
    }

    @Override
    public int hashCode() {
        return Objects.hash(date, open, close, adjclose, low, high, volume);
    }
}
