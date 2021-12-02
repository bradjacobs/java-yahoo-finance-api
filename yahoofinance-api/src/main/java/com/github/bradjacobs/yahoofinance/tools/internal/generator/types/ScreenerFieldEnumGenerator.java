/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Generates the "ScreenerField" enum class.
 */
// TODO - the enums really has too much clutter.  It should be reorganized and/or pruned.
public class ScreenerFieldEnumGenerator extends AbstractFieldEnumGenerator
{
    @Override
    protected String getUrl() {
        return "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US";
    }

    @Override
    protected String getOutputClassName() {
        return "ScreenerField";
    }

    @Override
    protected String getTemplateFileName() {
        return "field_template.txt";
    }






    // __SOME__ valid 'category' search criteria options (found from observation) .. for reference
    //    i.e.   https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?&region=US&category=popular_filters,debt_ratios
    //      popular_filters
    //      changes_in_price_and_market_cap
    //      changes_in_volume_and_ownership
    //      valuation_metric
    //      profitability_ratios_and_dividends
    //      debt_ratios
    //      liquidity_ratios
    //      eps_and_income_statement


    // don't include fields from the following categories.
    private static final Set<String> SKIP_CATEGORIES = new HashSet<>(Arrays.asList(
            "indexmembership",     // indexmembership
            "portfoliostatistics", // portfolioheldcount
            "ranking",
            "security_lifecycle",  // security_lifecycle
            "security_mapping",    // isin, new_listing_date
            "tickeralias"          // shortcuts
            //"userInsights"          // average_analyst_rating, bearish_count, bearish_proportion ....
    ));

    private static final Set<String> SKIP_FIELDS = new HashSet<>(Arrays.asList(
            "community_sentiment_date",
            "page_view_growth_weekly",
            "portfolio_sentiment_date",
            "total_portfolio_active_users",
            "top_fund_holder_names"
    ));


    @Override
    protected List<ScreenerFieldDefinition> filterFields(List<ScreenerFieldDefinition> fieldList)
    {
        return fieldList.stream()
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> sf.getCategory() == null || !SKIP_CATEGORIES.contains(sf.getCategory().getCategoryId()))
                .filter(sf ->  !SKIP_FIELDS.contains(sf.getFieldId()))
                .collect(Collectors.toList());
    }

/*
FULL CATEGORY LIST

    "balance_sheet",
    "cashflowstatement",
    "dividends_and_splits",
    "earnings",
    "esg_scores",
    "fair_value",
    "financials",
    "income",
    "indexmembership",
    "institutional_interest",
    "keystats",
    "morningstar_rating",
    "portfoliostatistics",
    "profile",
    "ranking",
    "sector_industry",
    "security_lifecycle",
    "security_mapping",
    "short_interest",
    "signals",
    "tickeralias",
    "userInsights",
    "valuation"
 */
}
