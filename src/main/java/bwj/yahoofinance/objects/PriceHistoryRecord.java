/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.objects;

/**
 * Represents a unique record entry from a CHART response
 *
 *   IMPORTANT NOTE:  playing around to see how this might look,
 *   may ultimately decide this class isn't worth having and remove this it.
 */
public class PriceHistoryRecord
{
    private Long timestamp;
    private Double open;
    private Double close;
    private Double adjclose;
    private Double low;
    private Double high;
    private Long volume;

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

    public Double getOpen() {
        return open;
    }

    public void setOpen(Double open) {
        this.open = open;
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

    public Double getLow() {
        return low;
    }

    public void setLow(Double low) {
        this.low = low;
    }

    public Double getHigh() {
        return high;
    }

    public void setHigh(Double high) {
        this.high = high;
    }

    public Long getVolume() {
        return volume;
    }

    public void setVolume(Long volume) {
        this.volume = volume;
    }
}
