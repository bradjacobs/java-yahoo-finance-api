package com.github.bradjacobs.yahoofinance.model.beta;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

public class IpoEventResult
{
    @JsonProperty("ticker")
    private String ticker;
    @JsonProperty("startdatetime")
    private Date startDateTime;
    @JsonProperty("companyshortname")
    private String companyName;
    @JsonProperty("amendeddate")
    private Date amendedDate;
    @JsonProperty("currencyname")
    private String currencyName;
    @JsonProperty("dealno")
    private String dealId;
    @JsonProperty("dealtype")
    @JsonAlias("action")
    private String dealType;
    @JsonProperty("exchange")
    private String exchange;
    @JsonProperty("filingdate")
    private Date filingDate;
    @JsonProperty("investortype")
    private String investorType;
    @JsonProperty("offerprice")
    @JsonAlias("price")
    private Double offerPrice;
    @JsonProperty("pricefrom")
    private Double priceFrom;
    @JsonProperty("priceto")
    private Double priceTo;
    @JsonProperty("quotetype")
    private String quoteType;
    @JsonProperty("shares")
    private Long shares;

    @JsonIgnore
    private final Map<String, Object> additionalProperties = new TreeMap<>(); // holds any extra unrecognized entries.

    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }

    public Date getStartDateTime() {
        return startDateTime;
    }

    public void setStartDateTime(Date startDateTime) {
        this.startDateTime = startDateTime;
    }

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }

    public Date getAmendedDate() {
        return amendedDate;
    }

    public void setAmendedDate(Date amendedDate) {
        this.amendedDate = amendedDate;
    }

    public String getCurrencyName() {
        return currencyName;
    }

    public void setCurrencyName(String currencyName) {
        this.currencyName = currencyName;
    }

    public String getDealId() {
        return dealId;
    }

    public void setDealId(String dealId) {
        this.dealId = dealId;
    }

    public String getDealType() {
        return dealType;
    }

    public void setDealType(String dealType) {
        this.dealType = dealType;
    }

    public String getExchange() {
        return exchange;
    }

    public void setExchange(String exchange) {
        this.exchange = exchange;
    }

    public Date getFilingDate() {
        return filingDate;
    }

    public void setFilingDate(Date filingDate) {
        this.filingDate = filingDate;
    }

    public String getInvestorType() {
        return investorType;
    }

    public void setInvestorType(String investorType) {
        this.investorType = investorType;
    }

    public Double getOfferPrice() {
        return offerPrice;
    }

    public void setOfferPrice(Double offerPrice) {
        this.offerPrice = offerPrice;
    }

    public Double getPriceFrom() {
        return priceFrom;
    }

    public void setPriceFrom(Double priceFrom) {
        this.priceFrom = priceFrom;
    }

    public Double getPriceTo() {
        return priceTo;
    }

    public void setPriceTo(Double priceTo) {
        this.priceTo = priceTo;
    }

    public String getQuoteType() {
        return quoteType;
    }

    public void setQuoteType(String quoteType) {
        this.quoteType = quoteType;
    }

    public Long getShares() {
        return shares;
    }

    public void setShares(Long shares) {
        this.shares = shares;
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
