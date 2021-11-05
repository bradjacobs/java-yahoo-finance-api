package com.github.bradjacobs.yahoofinance.model.beta;

import java.util.Date;

abstract public class AbstractVisualizationResult
{
    //     COUNT("count", true, "Document Count"),
    //    EVENTID("eventid", false, "Event ID"),
    //    STARTDATETIME("startdatetime", true, "Event Time"),
    //    TICKER("ticker", false, "Tickers"),


    private String ticker;
    private Date startdatetime;

    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }

    public Date getStartdatetime() {
        return startdatetime;
    }

    public void setStartdatetime(Date startdatetime) {
        this.startdatetime = startdatetime;
    }

}
