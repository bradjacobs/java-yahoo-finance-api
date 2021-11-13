package com.github.bradjacobs.yahoofinance.request.builder;

import java.util.Arrays;
import java.util.List;

public class IpoEventRequestBuilder extends AbstractVisualizationRequestBuilder<IpoEventRequestBuilder>
{
    private static final String ENTITY_TYPE = "ipo_info";
    private static final String SORT_FIELD = "startdatetime";  // for now sort fields are const

    private static final List<String> INCLUDE_FIELDS = Arrays.asList(
            "ticker", "companyshortname", "startdatetime",
            //"count", // NOTE: must _NOT_ include count... it will A) not return anything.... B)...mess up the order!!!
            "amendeddate", "currencyname", "dealno", "dealtype",
            "exchange", "filingdate", "investortype",
            "offerprice", "pricefrom", "priceto", "quotetype", "shares"
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
    protected IpoEventRequestBuilder getThis() {
        return this;
    }
}
