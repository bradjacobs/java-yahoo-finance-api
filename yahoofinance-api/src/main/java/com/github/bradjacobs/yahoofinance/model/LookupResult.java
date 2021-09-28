package com.github.bradjacobs.yahoofinance.model;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Map;
import java.util.TreeMap;

public class LookupResult
{
    private String symbol;
    private String shortName;
    private Float regularMarketPrice;
    private Float regularMarketChange;
    private Float regularMarketPercentChange;
    private String industryName;
    private String quoteType;
    private String exchange;
    private String industryLink;
    private Integer rank;

    @JsonIgnore
    private final Map<String, Object> additionalProperties = new TreeMap<>(); // holds any extra unrecognized entries.


    public String getSymbol()
    {
        return symbol;
    }

    public void setSymbol(String symbol)
    {
        this.symbol = symbol;
    }

    public String getShortName()
    {
        return shortName;
    }

    public void setShortName(String shortName)
    {
        this.shortName = shortName;
    }

    public Float getRegularMarketPrice()
    {
        return regularMarketPrice;
    }

    public void setRegularMarketPrice(Float regularMarketPrice)
    {
        this.regularMarketPrice = regularMarketPrice;
    }

    public Float getRegularMarketChange()
    {
        return regularMarketChange;
    }

    public void setRegularMarketChange(Float regularMarketChange)
    {
        this.regularMarketChange = regularMarketChange;
    }

    public Float getRegularMarketPercentChange()
    {
        return regularMarketPercentChange;
    }

    public void setRegularMarketPercentChange(Float regularMarketPercentChange)
    {
        this.regularMarketPercentChange = regularMarketPercentChange;
    }

    public String getIndustryName()
    {
        return industryName;
    }

    public void setIndustryName(String industryName)
    {
        this.industryName = industryName;
    }

    public String getQuoteType()
    {
        return quoteType;
    }

    public void setQuoteType(String quoteType)
    {
        this.quoteType = quoteType;
    }

    public String getExchange()
    {
        return exchange;
    }

    public void setExchange(String exchange)
    {
        this.exchange = exchange;
    }

    public String getIndustryLink()
    {
        return industryLink;
    }

    public void setIndustryLink(String industryLink)
    {
        this.industryLink = industryLink;
    }

    public Integer getRank()
    {
        return rank;
    }

    public void setRank(Integer rank)
    {
        this.rank = rank;
    }

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
