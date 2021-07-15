/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

abstract public class QueryResult implements Comparable<QueryResult>
{
    private String symbol;
    private String name;  // derived value.


    private String argusRating;
    private Double ask;
    private Long askSize;
    private String averageAnalystRating;
    private Long averageDailyVolume10Day;
    private Long averageDailyVolume3Month;
    private Double beta;
    private Double bid;
    private Long bidSize;
    private Double bookValue;
    private String currency;
    private String displayName;
    private Long dividendDate;
    private Double dividendRate;
    private Double dividendYield;
    private Double dividendsPerShare;
    private Long earningsTimestamp;
    private Long earningsTimestampEnd;
    private Long earningsTimestampStart;
    private Long ebitda;
    private Double epsCurrentYear;
    private Double epsForward;
    private Double epsTrailingTwelveMonths;
    private Boolean esgPopulated;
    private Long exDividendDate;
    private String exchange;
    private Long exchangeDataDelayedBy;
    private String exchangeTimezoneName;
    private String exchangeTimezoneShortName;
    private Double fiftyDayAverage;
    private Double fiftyDayAverageChange;
    private Double fiftyDayAverageChangePercent;
    private Double fiftyTwoWeekHigh;
    private Double fiftyTwoWeekHighChange;
    private Double fiftyTwoWeekHighChangePercent;
    private Double fiftyTwoWeekLow;
    private Double fiftyTwoWeekLowChange;
    private Double fiftyTwoWeekLowChangePercent;
    private String fiftyTwoWeekRange;
    private String financialCurrency;
    private Long firstTradeDateMilliseconds;
    private Double fiveYearGrowthRate;
    private Long floatShares;
    private Double forwardPE;
    private String fullExchangeName;
    private Long gmtOffSetMilliseconds;
    private Double heldPercentInsiders;
    private Double heldPercentInstitutions;
    private String language;
    private String longName;
    private String market;
    private Long marketCap;
    private String marketState;
    private String messageBoardId;
    private String morningstarEconomicMoatRating;
    private Double morningstarFairValue;
    private String morningstarIndustry;
    private String morningstarLongDescription;
    private String morningstarMoatTrendRating;
    private String morningstarReportRating;
    private String morningstarSector;
    private String morningstarStewardshipRating;
    private String morningstarUncertaintyRating;
    private Double netMargin;
    private Double oneYearEpsGrowth;
    private Double pegRatio;
    private Double postMarketChange;
    private Double postMarketChangePercent;
    private Double postMarketPrice;
    private Long postMarketTime;
    private Double priceEpsCurrentYear;
    private Long priceHint;
    private Double priceToBook;
    private Double priceToMorningstarFairValueRatio;
    private Double priceToSales;
    private String quoteSourceName;
    private String quoteType;
    private String region;
    private Double regularMarketChange;
    private Double regularMarketChangePercent;
    private Double regularMarketDayHigh;
    private Double regularMarketDayLow;
    private String regularMarketDayRange;
    private Double regularMarketOpen;
    private Double regularMarketPreviousClose;
    private Double regularMarketPrice;
    private Long regularMarketTime;
    private Long regularMarketVolume;
    private Long revenue;
    private Long sharesOutstanding;
    private Long sharesShort;
    private String shortName;
    private Double shortRatio;
    private Long sourceInterval;
    private Double targetPriceMean;
    private Long totalCash;
    private Boolean tradeable;
    private Double trailingAnnualDividendRate;
    private Double trailingAnnualDividendYield;
    private Double trailingPE;
    private Boolean triggerable;
    private Double twoHundredDayAverage;
    private Double twoHundredDayAverageChange;
    private Double twoHundredDayAverageChangePercent;
    private String uuid;

    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    public String getSymbol()
    {
        return symbol;
    }

    /**
     * Generically get most readable available 'name'
     * @return name
     */
    public String getName()
    {
        if (this.name != null) return this.name;
        else if (this.displayName != null) return this.displayName;
        else if (this.longName != null) return this.longName;
        else return this.shortName;
    }

    public String getArgusRating()
    {
        return argusRating;
    }

    public Double getAsk()
    {
        return ask;
    }

    public Long getAskSize()
    {
        return askSize;
    }

    public String getAverageAnalystRating()
    {
        return averageAnalystRating;
    }

    public Long getAverageDailyVolume10Day()
    {
        return averageDailyVolume10Day;
    }

    public Long getAverageDailyVolume3Month()
    {
        return averageDailyVolume3Month;
    }

    public Double getBeta()
    {
        return beta;
    }

    public Double getBid()
    {
        return bid;
    }

    public Long getBidSize()
    {
        return bidSize;
    }

    public Double getBookValue()
    {
        return bookValue;
    }

    public String getCurrency()
    {
        return currency;
    }

    public String getDisplayName()
    {
        return displayName;
    }

    public Long getDividendDate()
    {
        return dividendDate;
    }

    public Double getDividendRate()
    {
        return dividendRate;
    }

    public Double getDividendYield()
    {
        return dividendYield;
    }

    public Double getDividendsPerShare()
    {
        return dividendsPerShare;
    }

    public Long getEarningsTimestamp()
    {
        return earningsTimestamp;
    }

    public Long getEarningsTimestampEnd()
    {
        return earningsTimestampEnd;
    }

    public Long getEarningsTimestampStart()
    {
        return earningsTimestampStart;
    }

    public Long getEbitda()
    {
        return ebitda;
    }

    public Double getEpsCurrentYear()
    {
        return epsCurrentYear;
    }

    public Double getEpsForward()
    {
        return epsForward;
    }

    public Double getEpsTrailingTwelveMonths()
    {
        return epsTrailingTwelveMonths;
    }

    public Boolean getEsgPopulated()
    {
        return esgPopulated;
    }

    public Long getExDividendDate()
    {
        return exDividendDate;
    }

    public String getExchange()
    {
        return exchange;
    }

    public Long getExchangeDataDelayedBy()
    {
        return exchangeDataDelayedBy;
    }

    public String getExchangeTimezoneName()
    {
        return exchangeTimezoneName;
    }

    public String getExchangeTimezoneShortName()
    {
        return exchangeTimezoneShortName;
    }

    public Double getFiftyDayAverage()
    {
        return fiftyDayAverage;
    }

    public Double getFiftyDayAverageChange()
    {
        return fiftyDayAverageChange;
    }

    public Double getFiftyDayAverageChangePercent()
    {
        return fiftyDayAverageChangePercent;
    }

    public Double getFiftyTwoWeekHigh()
    {
        return fiftyTwoWeekHigh;
    }

    public Double getFiftyTwoWeekHighChange()
    {
        return fiftyTwoWeekHighChange;
    }

    public Double getFiftyTwoWeekHighChangePercent()
    {
        return fiftyTwoWeekHighChangePercent;
    }

    public Double getFiftyTwoWeekLow()
    {
        return fiftyTwoWeekLow;
    }

    public Double getFiftyTwoWeekLowChange()
    {
        return fiftyTwoWeekLowChange;
    }

    public Double getFiftyTwoWeekLowChangePercent()
    {
        return fiftyTwoWeekLowChangePercent;
    }

    public String getFiftyTwoWeekRange()
    {
        return fiftyTwoWeekRange;
    }

    public String getFinancialCurrency()
    {
        return financialCurrency;
    }

    public Long getFirstTradeDateMilliseconds()
    {
        return firstTradeDateMilliseconds;
    }

    public Double getFiveYearGrowthRate()
    {
        return fiveYearGrowthRate;
    }

    public Long getFloatShares()
    {
        return floatShares;
    }

    public Double getForwardPE()
    {
        return forwardPE;
    }

    public String getFullExchangeName()
    {
        return fullExchangeName;
    }

    public Long getGmtOffSetMilliseconds()
    {
        return gmtOffSetMilliseconds;
    }

    public Double getHeldPercentInsiders()
    {
        return heldPercentInsiders;
    }

    public Double getHeldPercentInstitutions()
    {
        return heldPercentInstitutions;
    }

    public String getLanguage()
    {
        return language;
    }

    public String getLongName()
    {
        return longName;
    }

    public String getMarket()
    {
        return market;
    }

    public Long getMarketCap()
    {
        return marketCap;
    }

    public String getMarketState()
    {
        return marketState;
    }

    public String getMessageBoardId()
    {
        return messageBoardId;
    }

    public String getMorningstarEconomicMoatRating()
    {
        return morningstarEconomicMoatRating;
    }

    public Double getMorningstarFairValue()
    {
        return morningstarFairValue;
    }

    public String getMorningstarIndustry()
    {
        return morningstarIndustry;
    }

    public String getMorningstarLongDescription()
    {
        return morningstarLongDescription;
    }

    public String getMorningstarMoatTrendRating()
    {
        return morningstarMoatTrendRating;
    }

    public String getMorningstarReportRating()
    {
        return morningstarReportRating;
    }

    public String getMorningstarSector()
    {
        return morningstarSector;
    }

    public String getMorningstarStewardshipRating()
    {
        return morningstarStewardshipRating;
    }

    public String getMorningstarUncertaintyRating()
    {
        return morningstarUncertaintyRating;
    }

    public Double getNetMargin()
    {
        return netMargin;
    }

    public Double getOneYearEpsGrowth()
    {
        return oneYearEpsGrowth;
    }

    public Double getPegRatio()
    {
        return pegRatio;
    }

    public Double getPostMarketChange()
    {
        return postMarketChange;
    }

    public Double getPostMarketChangePercent()
    {
        return postMarketChangePercent;
    }

    public Double getPostMarketPrice()
    {
        return postMarketPrice;
    }

    public Long getPostMarketTime()
    {
        return postMarketTime;
    }

    public Double getPriceEpsCurrentYear()
    {
        return priceEpsCurrentYear;
    }

    public Long getPriceHint()
    {
        return priceHint;
    }

    public Double getPriceToBook()
    {
        return priceToBook;
    }

    public Double getPriceToMorningstarFairValueRatio()
    {
        return priceToMorningstarFairValueRatio;
    }

    public Double getPriceToSales()
    {
        return priceToSales;
    }

    public String getQuoteSourceName()
    {
        return quoteSourceName;
    }

    public String getQuoteType()
    {
        return quoteType;
    }

    public String getRegion()
    {
        return region;
    }

    public Double getRegularMarketChange()
    {
        return regularMarketChange;
    }

    public Double getRegularMarketChangePercent()
    {
        return regularMarketChangePercent;
    }

    public Double getRegularMarketDayHigh()
    {
        return regularMarketDayHigh;
    }

    public Double getRegularMarketDayLow()
    {
        return regularMarketDayLow;
    }

    public String getRegularMarketDayRange()
    {
        return regularMarketDayRange;
    }

    public Double getRegularMarketOpen()
    {
        return regularMarketOpen;
    }

    public Double getRegularMarketPreviousClose()
    {
        return regularMarketPreviousClose;
    }

    public Double getRegularMarketPrice()
    {
        return regularMarketPrice;
    }

    public Long getRegularMarketTime()
    {
        return regularMarketTime;
    }

    public Long getRegularMarketVolume()
    {
        return regularMarketVolume;
    }

    public Long getRevenue()
    {
        return revenue;
    }

    public Long getSharesOutstanding()
    {
        return sharesOutstanding;
    }

    public Long getSharesShort()
    {
        return sharesShort;
    }

    public String getShortName()
    {
        return shortName;
    }

    public Double getShortRatio()
    {
        return shortRatio;
    }

    public Long getSourceInterval()
    {
        return sourceInterval;
    }

    public Double getTargetPriceMean()
    {
        return targetPriceMean;
    }

    public Long getTotalCash()
    {
        return totalCash;
    }

    public Boolean getTradeable()
    {
        return tradeable;
    }

    public Double getTrailingAnnualDividendRate()
    {
        return trailingAnnualDividendRate;
    }

    public Double getTrailingAnnualDividendYield()
    {
        return trailingAnnualDividendYield;
    }

    public Double getTrailingPE()
    {
        return trailingPE;
    }

    public Boolean getTriggerable()
    {
        return triggerable;
    }

    public Double getTwoHundredDayAverage()
    {
        return twoHundredDayAverage;
    }

    public Double getTwoHundredDayAverageChange()
    {
        return twoHundredDayAverageChange;
    }

    public Double getTwoHundredDayAverageChangePercent()
    {
        return twoHundredDayAverageChangePercent;
    }

    public String getUuid()
    {
        return uuid;
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


    @Override
    public int compareTo(@NotNull QueryResult other)
    {
        return this.symbol.compareTo(other.symbol);
    }
}
