package com.github.bradjacobs.yahoofinance.request.builder;

import java.util.Arrays;
import java.util.List;

//   https://query2.finance.yahoo.com/ws/screeners/v2/finance/screener/instrument/earnings/fields?lang=en-US&region=US&category=keystats%2Cfinancials%2Cvaluation%2Csector_industry%2Cesgscores%2Cincome%2Ccashflowstatement%2Cbalance_sheet%2Cearnings%2Cdividends_and_splits%2Cpopular_filters%2Cchanges_in_price_and_market_cap%2Cchanges_in_volume_and_ownership%2Cvaluation_metric%2Cprofitability_ratios_and_dividends%2Cdebt_ratios%2Cliquidity_ratios%2Ceps_and_income_statement%2Ccash_flow%2Cesg_scores%2Cshort_interest&enableNewCategories=true&corsDomain=finance.yahoo.com

public class EarningsEventRequestBuilder extends AbstractVisualizationRequestBuilder<EarningsEventRequestBuilder>
{
    private static final String ENTITY_TYPE = "earnings";
    private static final String SORT_FIELD = "companyshortname";  // for now sort fields are const

    private static final List<String> INCLUDE_FIELDS = Arrays.asList(
            "ticker", "companyshortname", "startdatetime",
            //"count", // NOTE: must _NOT_ include count... it will A) not return anything.... B)...mess up the order!!!
            "startdatetimetype", "epsestimate", "epsactual", "epssurprisepct",
            "sector", "industry", "beta", "isin", "exchange",
            "dateisestimate", "epsconsensus", "eventname", "eventtype",
            "fiscalyear", "quarter", "timeZoneShortName"
    );

    // ticker, companyshortname, sector, industry, beta, isin, exchange  (timeZoneShortName)

    @Override
    protected String getEntityType() {
        return ENTITY_TYPE;
    }

    @Override
    protected String getSortField() {
        return SORT_FIELD;
    }

    @Override
    protected List<String> getIncludeFields() {
        return INCLUDE_FIELDS;
    }

    @Override
    protected EarningsEventRequestBuilder getThis() {
        return this;
    }
}