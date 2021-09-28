package com.github.bradjacobs.yahoofinance.model.beta;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Map;
import java.util.TreeMap;

/**
 * TODO :  EXPERIMENT!!
 * a _SUBSET_ of possible timeSeries values.
 */
public class BaseTimeSeriesResult
{
    // income statement
    private Long basicAverageShares;
    private Number basicEPS;
    private Long dilutedAverageShares;
    private Number dilutedEPS;
    private Long ebit;
    private Long ebitda;
    private Long interestExpense;
    private Long netIncome;
    private Long pretaxIncome;
    private Long taxProvision;
    private Long totalRevenue;


    // balance sheet
    private Long cashAndCashEquivalents;
    private Long cashCashEquivalentsAndShortTermInvestments;
    private Long currentAssets;
    private Long currentDebt;
    private Long currentDebtAndCapitalLeaseObligation;
    private Long currentLiabilities;
    private Long longTermDebt;
    private Long longTermDebtAndCapitalLeaseObligation;
    private Long retainedEarnings;
    private Long stockholdersEquity;
    private Long totalAssets;
    private Long totalDebt;
    private Long totalLiabilitiesNetMinorityInterest;
    private Long workingCapital;


    // cash flow statement
    private Long capitalExpenditure;
    private Long cashDividendsPaid;
    private Long changeInWorkingCapital;
    private Long depreciationAmortizationDepletion;
    private Long freeCashFlow;


    // valuation
    private Long enterpriseValue;
    private Number enterprisesValueEBITDARatio;
    private Number enterprisesValueRevenueRatio;
    private Number forwardPeRatio;
    private Long marketCap;
    private Number pbRatio;
    private Number peRatio;
    private Number pegRatio;
    private Number psRatio;



    @JsonIgnore
    private final Map<String, Object> additionalProperties = new TreeMap<>();


    @JsonIgnore
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }


    public Long getBasicAverageShares() {
        return basicAverageShares;
    }

    public void setBasicAverageShares(Long basicAverageShares) {
        this.basicAverageShares = basicAverageShares;
    }

    public Number getBasicEPS() {
        return basicEPS;
    }

    public void setBasicEPS(Number basicEPS) {
        this.basicEPS = basicEPS;
    }

    public Long getDilutedAverageShares() {
        return dilutedAverageShares;
    }

    public void setDilutedAverageShares(Long dilutedAverageShares) {
        this.dilutedAverageShares = dilutedAverageShares;
    }

    public Number getDilutedEPS() {
        return dilutedEPS;
    }

    public void setDilutedEPS(Number dilutedEPS) {
        this.dilutedEPS = dilutedEPS;
    }

    public Long getEbit() {
        return ebit;
    }

    public void setEbit(Long ebit) {
        this.ebit = ebit;
    }

    public Long getEbitda() {
        return ebitda;
    }

    public void setEbitda(Long ebitda) {
        this.ebitda = ebitda;
    }

    public Long getInterestExpense() {
        return interestExpense;
    }

    public void setInterestExpense(Long interestExpense) {
        this.interestExpense = interestExpense;
    }

    public Long getNetIncome() {
        return netIncome;
    }

    public void setNetIncome(Long netIncome) {
        this.netIncome = netIncome;
    }

    public Long getPretaxIncome() {
        return pretaxIncome;
    }

    public void setPretaxIncome(Long pretaxIncome) {
        this.pretaxIncome = pretaxIncome;
    }

    public Long getTaxProvision() {
        return taxProvision;
    }

    public void setTaxProvision(Long taxProvision) {
        this.taxProvision = taxProvision;
    }

    public Long getTotalRevenue() {
        return totalRevenue;
    }

    public void setTotalRevenue(Long totalRevenue) {
        this.totalRevenue = totalRevenue;
    }

    public Long getCashAndCashEquivalents() {
        return cashAndCashEquivalents;
    }

    public void setCashAndCashEquivalents(Long cashAndCashEquivalents) {
        this.cashAndCashEquivalents = cashAndCashEquivalents;
    }

    public Long getCashCashEquivalentsAndShortTermInvestments() {
        return cashCashEquivalentsAndShortTermInvestments;
    }

    public void setCashCashEquivalentsAndShortTermInvestments(Long cashCashEquivalentsAndShortTermInvestments) {
        this.cashCashEquivalentsAndShortTermInvestments = cashCashEquivalentsAndShortTermInvestments;
    }

    public Long getCurrentAssets() {
        return currentAssets;
    }

    public void setCurrentAssets(Long currentAssets) {
        this.currentAssets = currentAssets;
    }

    public Long getCurrentDebt() {
        return currentDebt;
    }

    public void setCurrentDebt(Long currentDebt) {
        this.currentDebt = currentDebt;
    }

    public Long getCurrentDebtAndCapitalLeaseObligation() {
        return currentDebtAndCapitalLeaseObligation;
    }

    public void setCurrentDebtAndCapitalLeaseObligation(Long currentDebtAndCapitalLeaseObligation) {
        this.currentDebtAndCapitalLeaseObligation = currentDebtAndCapitalLeaseObligation;
    }

    public Long getCurrentLiabilities() {
        return currentLiabilities;
    }

    public void setCurrentLiabilities(Long currentLiabilities) {
        this.currentLiabilities = currentLiabilities;
    }

    public Long getLongTermDebt() {
        return longTermDebt;
    }

    public void setLongTermDebt(Long longTermDebt) {
        this.longTermDebt = longTermDebt;
    }

    public Long getLongTermDebtAndCapitalLeaseObligation() {
        return longTermDebtAndCapitalLeaseObligation;
    }

    public void setLongTermDebtAndCapitalLeaseObligation(Long longTermDebtAndCapitalLeaseObligation) {
        this.longTermDebtAndCapitalLeaseObligation = longTermDebtAndCapitalLeaseObligation;
    }

    public Long getRetainedEarnings() {
        return retainedEarnings;
    }

    public void setRetainedEarnings(Long retainedEarnings) {
        this.retainedEarnings = retainedEarnings;
    }

    public Long getStockholdersEquity() {
        return stockholdersEquity;
    }

    public void setStockholdersEquity(Long stockholdersEquity) {
        this.stockholdersEquity = stockholdersEquity;
    }

    public Long getTotalAssets() {
        return totalAssets;
    }

    public void setTotalAssets(Long totalAssets) {
        this.totalAssets = totalAssets;
    }

    public Long getTotalDebt() {
        return totalDebt;
    }

    public void setTotalDebt(Long totalDebt) {
        this.totalDebt = totalDebt;
    }

    public Long getTotalLiabilitiesNetMinorityInterest() {
        return totalLiabilitiesNetMinorityInterest;
    }

    public void setTotalLiabilitiesNetMinorityInterest(Long totalLiabilitiesNetMinorityInterest) {
        this.totalLiabilitiesNetMinorityInterest = totalLiabilitiesNetMinorityInterest;
    }

    public Long getWorkingCapital() {
        return workingCapital;
    }

    public void setWorkingCapital(Long workingCapital) {
        this.workingCapital = workingCapital;
    }

    public Long getCapitalExpenditure() {
        return capitalExpenditure;
    }

    public void setCapitalExpenditure(Long capitalExpenditure) {
        this.capitalExpenditure = capitalExpenditure;
    }

    public Long getCashDividendsPaid() {
        return cashDividendsPaid;
    }

    public void setCashDividendsPaid(Long cashDividendsPaid) {
        this.cashDividendsPaid = cashDividendsPaid;
    }

    public Long getChangeInWorkingCapital() {
        return changeInWorkingCapital;
    }

    public void setChangeInWorkingCapital(Long changeInWorkingCapital) {
        this.changeInWorkingCapital = changeInWorkingCapital;
    }

    public Long getDepreciationAmortizationDepletion() {
        return depreciationAmortizationDepletion;
    }

    public void setDepreciationAmortizationDepletion(Long depreciationAmortizationDepletion) {
        this.depreciationAmortizationDepletion = depreciationAmortizationDepletion;
    }

    public Long getFreeCashFlow() {
        return freeCashFlow;
    }

    public void setFreeCashFlow(Long freeCashFlow) {
        this.freeCashFlow = freeCashFlow;
    }

    public Long getEnterpriseValue() {
        return enterpriseValue;
    }

    public void setEnterpriseValue(Long enterpriseValue) {
        this.enterpriseValue = enterpriseValue;
    }

    public Number getEnterprisesValueEBITDARatio() {
        return enterprisesValueEBITDARatio;
    }

    public void setEnterprisesValueEBITDARatio(Number enterprisesValueEBITDARatio) {
        this.enterprisesValueEBITDARatio = enterprisesValueEBITDARatio;
    }

    public Number getEnterprisesValueRevenueRatio() {
        return enterprisesValueRevenueRatio;
    }

    public void setEnterprisesValueRevenueRatio(Number enterprisesValueRevenueRatio) {
        this.enterprisesValueRevenueRatio = enterprisesValueRevenueRatio;
    }

    public Number getForwardPeRatio() {
        return forwardPeRatio;
    }

    public void setForwardPeRatio(Number forwardPeRatio) {
        this.forwardPeRatio = forwardPeRatio;
    }

    public Long getMarketCap() {
        return marketCap;
    }

    public void setMarketCap(Long marketCap) {
        this.marketCap = marketCap;
    }

    public Number getPbRatio() {
        return pbRatio;
    }

    public void setPbRatio(Number pbRatio) {
        this.pbRatio = pbRatio;
    }

    public Number getPeRatio() {
        return peRatio;
    }

    public void setPeRatio(Number peRatio) {
        this.peRatio = peRatio;
    }

    public Number getPegRatio() {
        return pegRatio;
    }

    public void setPegRatio(Number pegRatio) {
        this.pegRatio = pegRatio;
    }

    public Number getPsRatio() {
        return psRatio;
    }

    public void setPsRatio(Number psRatio) {
        this.psRatio = psRatio;
    }
}
