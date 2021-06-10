package com.github.bradjacobs.yahoofinance.model;

public class TimeEntry
{
    private Long timestamp;
    private String dataId;
    private String asOfDate;
    private String periodType;
    private String currencyCode;
    private Number reportedValue;

    public Long getTimestamp()
    {
        return timestamp;
    }

    public String getDataId()
    {
        return dataId;
    }

    public String getAsOfDate()
    {
        return asOfDate;
    }

    public String getPeriodType()
    {
        return periodType;
    }

    public String getCurrencyCode()
    {
        return currencyCode;
    }

    public Number getReportedValue()
    {
        return reportedValue;
    }

}
