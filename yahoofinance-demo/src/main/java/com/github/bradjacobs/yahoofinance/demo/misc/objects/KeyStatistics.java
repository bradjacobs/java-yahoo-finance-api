package com.github.bradjacobs.yahoofinance.demo.misc.objects;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.HashMap;
import java.util.Map;

/**
 * Generic pojo to hold _SOME_ data from the modules 'keyStatistics'
 */
public class KeyStatistics
{
    private String ticker;

    private Long enterpriseValue;
    private Double forwardPE;
    private Double profitMargins;
    private Long floatShares;
    private Long sharesOutstanding;
    private Long sharesShort;
    private Long dateShortInterest;
    private Double sharesPercentSharesOut;
    private Double shortRatio;
    private Double shortPercentOfFloat;
    private Double beta;
    private Double bookValue;
    private Double priceToBook;

    private Long lastFiscalYearEnd;  // timestamp
    private Long nextFiscalYearEnd;  // timestamp
    private Long mostRecentQuarter;  // timestamp
    private Double earningsQuarterlyGrowth;
    private Long netIncomeToCommon;
    private Double trailingEps;
    private Double forwardEps;
    private Double pegRatio;

    private String lastSplitFactor;
    private Long lastSplitDate;  // timestamp

    private Double enterpriseToRevenue;
    private Double enterpriseToEbitda;

    @JsonProperty("52WeekChange")
    private Double fiftyTwoWeekChange;
    private Double sandP52WeekChange;
    private Double lastDividendValue;
    private Long lastDividendDate;  // timestamp


    // map that holds all 'extra' fields that didn't get mapped.
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();



    public String getTicker()
    {
        return ticker;
    }

    public void setTicker(String ticker)
    {
        this.ticker = ticker;
    }

    public Long getEnterpriseValue()
    {
        return enterpriseValue;
    }

    public void setEnterpriseValue(Long enterpriseValue)
    {
        this.enterpriseValue = enterpriseValue;
    }

    public Double getForwardPE()
    {
        return forwardPE;
    }

    public void setForwardPE(Double forwardPE)
    {
        this.forwardPE = forwardPE;
    }

    public Double getProfitMargins()
    {
        return profitMargins;
    }

    public void setProfitMargins(Double profitMargins)
    {
        this.profitMargins = profitMargins;
    }

    public Long getFloatShares()
    {
        return floatShares;
    }

    public void setFloatShares(Long floatShares)
    {
        this.floatShares = floatShares;
    }

    public Long getSharesOutstanding()
    {
        return sharesOutstanding;
    }

    public void setSharesOutstanding(Long sharesOutstanding)
    {
        this.sharesOutstanding = sharesOutstanding;
    }

    public Long getSharesShort()
    {
        return sharesShort;
    }

    public void setSharesShort(Long sharesShort)
    {
        this.sharesShort = sharesShort;
    }

    public Long getDateShortInterest()
    {
        return dateShortInterest;
    }

    public void setDateShortInterest(Long dateShortInterest)
    {
        this.dateShortInterest = dateShortInterest;
    }

    public Double getSharesPercentSharesOut()
    {
        return sharesPercentSharesOut;
    }

    public void setSharesPercentSharesOut(Double sharesPercentSharesOut)
    {
        this.sharesPercentSharesOut = sharesPercentSharesOut;
    }

    public Double getShortRatio()
    {
        return shortRatio;
    }

    public void setShortRatio(Double shortRatio)
    {
        this.shortRatio = shortRatio;
    }

    public Double getShortPercentOfFloat()
    {
        return shortPercentOfFloat;
    }

    public void setShortPercentOfFloat(Double shortPercentOfFloat)
    {
        this.shortPercentOfFloat = shortPercentOfFloat;
    }

    public Double getBeta()
    {
        return beta;
    }

    public void setBeta(Double beta)
    {
        this.beta = beta;
    }

    public Double getBookValue()
    {
        return bookValue;
    }

    public void setBookValue(Double bookValue)
    {
        this.bookValue = bookValue;
    }

    public Double getPriceToBook()
    {
        return priceToBook;
    }

    public void setPriceToBook(Double priceToBook)
    {
        this.priceToBook = priceToBook;
    }

    public Long getLastFiscalYearEnd()
    {
        return lastFiscalYearEnd;
    }

    public void setLastFiscalYearEnd(Long lastFiscalYearEnd)
    {
        this.lastFiscalYearEnd = lastFiscalYearEnd;
    }

    public Long getNextFiscalYearEnd()
    {
        return nextFiscalYearEnd;
    }

    public void setNextFiscalYearEnd(Long nextFiscalYearEnd)
    {
        this.nextFiscalYearEnd = nextFiscalYearEnd;
    }

    public Long getMostRecentQuarter()
    {
        return mostRecentQuarter;
    }

    public void setMostRecentQuarter(Long mostRecentQuarter)
    {
        this.mostRecentQuarter = mostRecentQuarter;
    }

    public Double getEarningsQuarterlyGrowth()
    {
        return earningsQuarterlyGrowth;
    }

    public void setEarningsQuarterlyGrowth(Double earningsQuarterlyGrowth)
    {
        this.earningsQuarterlyGrowth = earningsQuarterlyGrowth;
    }

    public Long getNetIncomeToCommon()
    {
        return netIncomeToCommon;
    }

    public void setNetIncomeToCommon(Long netIncomeToCommon)
    {
        this.netIncomeToCommon = netIncomeToCommon;
    }

    public Double getTrailingEps()
    {
        return trailingEps;
    }

    public void setTrailingEps(Double trailingEps)
    {
        this.trailingEps = trailingEps;
    }

    public Double getForwardEps()
    {
        return forwardEps;
    }

    public void setForwardEps(Double forwardEps)
    {
        this.forwardEps = forwardEps;
    }

    public Double getPegRatio()
    {
        return pegRatio;
    }

    public void setPegRatio(Double pegRatio)
    {
        this.pegRatio = pegRatio;
    }

    public String getLastSplitFactor()
    {
        return lastSplitFactor;
    }

    public void setLastSplitFactor(String lastSplitFactor)
    {
        this.lastSplitFactor = lastSplitFactor;
    }

    public Long getLastSplitDate()
    {
        return lastSplitDate;
    }

    public void setLastSplitDate(Long lastSplitDate)
    {
        this.lastSplitDate = lastSplitDate;
    }

    public Double getEnterpriseToRevenue()
    {
        return enterpriseToRevenue;
    }

    public void setEnterpriseToRevenue(Double enterpriseToRevenue)
    {
        this.enterpriseToRevenue = enterpriseToRevenue;
    }

    public Double getEnterpriseToEbitda()
    {
        return enterpriseToEbitda;
    }

    public void setEnterpriseToEbitda(Double enterpriseToEbitda)
    {
        this.enterpriseToEbitda = enterpriseToEbitda;
    }

    public Double getFiftyTwoWeekChange()
    {
        return fiftyTwoWeekChange;
    }

    public void setFiftyTwoWeekChange(Double fiftyTwoWeekChange)
    {
        this.fiftyTwoWeekChange = fiftyTwoWeekChange;
    }

    public Double getSandP52WeekChange()
    {
        return sandP52WeekChange;
    }

    public void setSandP52WeekChange(Double sandP52WeekChange)
    {
        this.sandP52WeekChange = sandP52WeekChange;
    }

    public Double getLastDividendValue()
    {
        return lastDividendValue;
    }

    public void setLastDividendValue(Double lastDividendValue)
    {
        this.lastDividendValue = lastDividendValue;
    }

    public Long getLastDividendDate()
    {
        return lastDividendDate;
    }

    public void setLastDividendDate(Long lastDividendDate)
    {
        this.lastDividendDate = lastDividendDate;
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
