package com.github.bradjacobs.yahoofinance.model.beta;

import java.util.Date;

public class EarningsVisualizationResult
{
    private String ticker;
    private Date startdatetime;
    private String companyshortname;

    // example values:    "BMO", "AMC", "TNS", "TAS"
    private String startdatetimetype;   // "Event Start Time"

    private Long count;
    private Boolean dateisestimate;
    private Date enddatetime;
    private Double epsactual;
    private Number epsconsensus;
    private Number epsestimate;
    private Number epssurprisepct;
    private String eventId;
    private String eventname;
    private String eventtype;
    private String fiscalyear;  // defn says it's a string
    private String quarter;  // defn says it's a string

    private String timeZoneShortName;
    private Long gmtOffsetMilliSeconds;


}
