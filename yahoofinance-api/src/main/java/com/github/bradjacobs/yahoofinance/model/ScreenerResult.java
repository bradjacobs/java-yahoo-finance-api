/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;


/**
 * Hold data for a Screener result entry
 */

/* NOTE: it looks like Quote & Screener both return the same result type,
            thus have a common abstract class.
        But exception discovered when calling the 'premium' endpoint for Screener
        can result with extra fields.   For now will keep the inheritance,
        but it is still subject to removal.
 */
public class ScreenerResult extends QueryResult
{
    // ** Fields below only returned with "Premium" endpoint **
    private Double estimatedEarningsGrowth;
    private Double estimatedRevenueGrowth;
    private Double fairValue;
    private String morningstarCurrentRating;  // todo: should be number?
    private Double morningstarLastClosePriceToFairValue;
    private String morningstarPreviousRating; // todo: should be number?
    private String morningstarRatingChange;
    private Double priceToFairValue;
    private Double rorPercent;
    private String valueDescription;

    public Double getEstimatedEarningsGrowth() {
        return estimatedEarningsGrowth;
    }

    public Double getEstimatedRevenueGrowth() {
        return estimatedRevenueGrowth;
    }

    public Double getFairValue() {
        return fairValue;
    }

    public String getMorningstarCurrentRating() {
        return morningstarCurrentRating;
    }

    public Double getMorningstarLastClosePriceToFairValue() {
        return morningstarLastClosePriceToFairValue;
    }

    public String getMorningstarPreviousRating() {
        return morningstarPreviousRating;
    }

    public String getMorningstarRatingChange() {
        return morningstarRatingChange;
    }

    public Double getPriceToFairValue() {
        return priceToFairValue;
    }

    public Double getRorPercent() {
        return rorPercent;
    }

    public String getValueDescription() {
        return valueDescription;
    }
}
