package bwj.yahoofinance.objects;

public enum Range
{
    ONE_DAY("1d"),
    FIVE_DAYS("5d"),
    ONE_MONTH("1mo"),
    THREE_MONTHS("3mo"),
    SIX_MONTHS("6mo"),
    ONE_YEAR("1y"),
    TWO_YEARS("2y"),
    FIVE_YEARS("5y"),
    TEN_YEARS("10y"),
    YTD("ytd"),
    MAX("max");


    private final String value;

    Range(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
