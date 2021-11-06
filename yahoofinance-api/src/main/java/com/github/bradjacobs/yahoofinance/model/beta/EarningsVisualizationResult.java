package com.github.bradjacobs.yahoofinance.model.beta;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

public class EarningsVisualizationResult
{
    private String ticker;
    private Date startdatetime;
    private String companyshortname;

    // example values:    "BMO", "AMC", "TNS", "TAS"
    private String startdatetimetype;   // "Event Start Time"

    private Long count;  // always blank
    private Boolean dateisestimate;
    private Date enddatetime;  // always blank
    private Double epsactual;
    private Number epsconsensus;
    private Number epsestimate;
    private Number epssurprisepct;
    private String eventname;
    private String eventtype;   // not sure what this is...sometimes 'null'...sometimes 2
    private String fiscalyear;  // defn says it's a string     "2021"
    private String quarter;  // defn says it's a string      "3"

    private String timeZoneShortName;
//    private Long gmtOffsetMilliSeconds;

    @JsonIgnore
    private final Map<String, Object> additionalProperties = new TreeMap<>(); // holds any extra unrecognized entries.


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

    public String getCompanyshortname() {
        return companyshortname;
    }

    public void setCompanyshortname(String companyshortname) {
        this.companyshortname = companyshortname;
    }

    public String getStartdatetimetype() {
        return startdatetimetype;
    }

    public void setStartdatetimetype(String startdatetimetype) {
        this.startdatetimetype = startdatetimetype;
    }

    public Long getCount() {
        return count;
    }

    public void setCount(Long count) {
        this.count = count;
    }

    public Boolean getDateisestimate() {
        return dateisestimate;
    }

    public void setDateisestimate(Boolean dateisestimate) {
        this.dateisestimate = dateisestimate;
    }

    public Date getEnddatetime() {
        return enddatetime;
    }

    public void setEnddatetime(Date enddatetime) {
        this.enddatetime = enddatetime;
    }

    public Double getEpsactual() {
        return epsactual;
    }

    public void setEpsactual(Double epsactual) {
        this.epsactual = epsactual;
    }

    public Number getEpsconsensus() {
        return epsconsensus;
    }

    public void setEpsconsensus(Number epsconsensus) {
        this.epsconsensus = epsconsensus;
    }

    public Number getEpsestimate() {
        return epsestimate;
    }

    public void setEpsestimate(Number epsestimate) {
        this.epsestimate = epsestimate;
    }

    public Number getEpssurprisepct() {
        return epssurprisepct;
    }

    public void setEpssurprisepct(Number epssurprisepct) {
        this.epssurprisepct = epssurprisepct;
    }

    public String getEventname() {
        return eventname;
    }

    public void setEventname(String eventname) {
        this.eventname = eventname;
    }

    public String getEventtype() {
        return eventtype;
    }

    public void setEventtype(String eventtype) {
        this.eventtype = eventtype;
    }

    public String getFiscalyear() {
        return fiscalyear;
    }

    public void setFiscalyear(String fiscalyear) {
        this.fiscalyear = fiscalyear;
    }

    public String getQuarter() {
        return quarter;
    }

    public void setQuarter(String quarter) {
        this.quarter = quarter;
    }

    public String getTimeZoneShortName() {
        return timeZoneShortName;
    }

    public void setTimeZoneShortName(String timeZoneShortName) {
        this.timeZoneShortName = timeZoneShortName;
    }

//    public Long getGmtOffsetMilliSeconds() {
//        return gmtOffsetMilliSeconds;
//    }
//
//    public void setGmtOffsetMilliSeconds(Long gmtOffsetMilliSeconds) {
//        this.gmtOffsetMilliSeconds = gmtOffsetMilliSeconds;
//    }

    @JsonAnyGetter
    private Map<String, Object> getAdditionalProperties()
    {
        return this.additionalProperties;
    }

    @JsonAnySetter
    private void setAdditionalProperty(String name, Object value)
    {
        this.additionalProperties.put(name, value);
    }

}
