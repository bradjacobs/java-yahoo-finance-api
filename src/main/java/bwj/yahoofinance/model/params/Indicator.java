/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.model.params;

public enum Indicator
{
    CLOSE("close"),
    ADJ_CLOSE("adjclose");

    private final String value;

    Indicator(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
