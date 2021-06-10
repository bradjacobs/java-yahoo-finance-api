package com.github.bradjacobs.yahoofinance.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class TimeSeriesResult
{
    @JsonProperty("fieldType")
    private String fieldType;

    @JsonProperty("timeEntries")
    private List<TimeEntry> timeEntries;

    public String getFieldType()
    {
        return fieldType;
    }

    public List<TimeEntry> getTimeEntries()
    {
        return timeEntries;
    }

}
