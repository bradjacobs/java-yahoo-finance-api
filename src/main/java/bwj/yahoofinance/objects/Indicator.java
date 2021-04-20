package bwj.yahoofinance.objects;

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
