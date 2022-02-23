package com.github.bradjacobs.yahoofinance.model;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class EarningsEventResult
{
    @JsonProperty("ticker")
    private String ticker;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonProperty("startdatetime")
    private LocalDateTime startDateTime;

    @JsonProperty("companyshortname")
    private String companyName;

    @JsonProperty("startdatetimetype")
    private String startDateTimeType;   // "Event Start Time"  (example values: "BMO", "AMC", "TNS", "TAS")

    @JsonProperty("dateisestimate")
    private Boolean dateIsEstimate;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonProperty("enddatetime")
    private LocalDateTime endDateTime;  // always blank??

    @JsonProperty("epsactual")
    private Double epsActual;

    @JsonProperty("epsconsensus")
    private Number epsConsensus;

    @JsonProperty("epsestimate")
    private Number epsEstimate;

    @JsonProperty("epssurprisepct")
    private Number epsSurprisePct;

    @JsonProperty("eventtype")
    private String eventType;   // only seen values of 'null' and '2' (so far)

    @JsonProperty("eventname")
    private String eventName;

    @JsonProperty("sector")
    private String sector;

    @JsonProperty("industry")
    private String industry;

    @JsonProperty("beta")
    private Double beta;

    @JsonProperty("isin")
    private String isin;

    @JsonProperty("exchange")
    private String exchange;

    @JsonProperty("fiscalyear")
    private Long fiscalYear;

    @JsonProperty("quarter")
    private Long quarter;

    @JsonProperty("timeZoneShortName")
    private String timeZoneShortName;

    @JsonIgnore
    private final Map<String, Object> additionalProperties = new TreeMap<>(); // holds any extra unrecognized entries.

    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }

    public LocalDateTime getStartDateTime() {
        return startDateTime;
    }

    public void setStartDateTime(LocalDateTime startDateTime) {
        this.startDateTime = startDateTime;
    }

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }

    public String getStartDateTimeType() {
        return startDateTimeType;
    }

    public void setStartDateTimeType(String startDateTimeType) {
        this.startDateTimeType = startDateTimeType;
    }

    public Boolean getDateIsEstimate() {
        return dateIsEstimate;
    }

    public void setDateIsEstimate(Boolean dateIsEstimate) {
        this.dateIsEstimate = dateIsEstimate;
    }

    public LocalDateTime getEndDateTime() {
        return endDateTime;
    }

    public void setEndDateTime(LocalDateTime endDateTime) {
        this.endDateTime = endDateTime;
    }

    public Double getEpsActual() {
        return epsActual;
    }

    public void setEpsActual(Double epsActual) {
        this.epsActual = epsActual;
    }

    public Number getEpsConsensus() {
        return epsConsensus;
    }

    public void setEpsConsensus(Number epsConsensus) {
        this.epsConsensus = epsConsensus;
    }

    public Number getEpsEstimate() {
        return epsEstimate;
    }

    public void setEpsEstimate(Number epsEstimate) {
        this.epsEstimate = epsEstimate;
    }

    public Number getEpsSurprisePct() {
        return epsSurprisePct;
    }

    public void setEpsSurprisePct(Number epsSurprisePct) {
        this.epsSurprisePct = epsSurprisePct;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public String getEventName() {
        return eventName;
    }

    public void setEventName(String eventName) {
        this.eventName = eventName;
    }

    public String getSector() {
        return sector;
    }

    public void setSector(String sector) {
        this.sector = sector;
    }

    public String getIndustry() {
        return industry;
    }

    public void setIndustry(String industry) {
        this.industry = industry;
    }

    public Double getBeta() {
        return beta;
    }

    public void setBeta(Double beta) {
        this.beta = beta;
    }

    public String getIsin() {
        return isin;
    }

    public void setIsin(String isin) {
        this.isin = isin;
    }

    public String getExchange() {
        return exchange;
    }

    public void setExchange(String exchange) {
        this.exchange = exchange;
    }

    public Long getFiscalYear() {
        return fiscalYear;
    }

    public void setFiscalYear(Long fiscalYear) {
        this.fiscalYear = fiscalYear;
    }

    public Long getQuarter() {
        return quarter;
    }

    public void setQuarter(Long quarter) {
        this.quarter = quarter;
    }

    public String getTimeZoneShortName() {
        return timeZoneShortName;
    }

    public void setTimeZoneShortName(String timeZoneShortName) {
        this.timeZoneShortName = timeZoneShortName;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties()
    {
        return this.additionalProperties;
    }

    @JsonAnySetter
    private void setAdditionalProperty(String name, Object value)
    {
        this.additionalProperties.put(name, value);
    }
}
