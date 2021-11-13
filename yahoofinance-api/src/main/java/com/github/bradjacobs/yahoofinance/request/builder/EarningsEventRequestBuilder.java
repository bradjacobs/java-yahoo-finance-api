package com.github.bradjacobs.yahoofinance.request.builder;

import java.util.Arrays;
import java.util.List;

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