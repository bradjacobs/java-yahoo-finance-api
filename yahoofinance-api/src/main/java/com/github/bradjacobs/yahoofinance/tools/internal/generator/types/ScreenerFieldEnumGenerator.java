/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.Category;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Generates the "ScreenerField" enum class.
 */
// TODO - the enums really has too much clutter.  It should be reorganized and/or pruned.
public class ScreenerFieldEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "screener_field_template.txt";

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


    // don't include fiels from the following categories.
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


    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US";

    private static final JsonMapper mapper = JsonMapperFactory.getPrettyMapper();


    @Override
    protected String getTemplateFileName()
    {
        return TEMPLATE_NAME;
    }

    @Override
    protected String getUrl() {
        return URL;
    }


    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<Map<String, Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].fields.*");
        ScreenerFieldDefinition[] fields = mapper.convertValue(listOfMaps, ScreenerFieldDefinition[].class);

        // filter out fields want to ignore.
        List<ScreenerFieldDefinition> filteredList = Arrays.stream(fields)
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> sf.getCategory() == null || !SKIP_CATEGORIES.contains(sf.getCategory().getCategoryId()))
                .filter(sf ->  !SKIP_FIELDS.contains(sf.getFieldId()))
                .collect(Collectors.toList());

        // after initial filtering, make 2 lists non-premium and premium
        List<ScreenerFieldDefinition> basicFieldList = filteredList.stream()
            .filter(sf -> !sf.getIsPremium())
            .collect(Collectors.toList());

        List<ScreenerFieldDefinition> premiumFieldList = filteredList.stream()
            .filter(sf -> sf.getIsPremium())
            .collect(Collectors.toList());

        List<EnumInfo> enumInfoList = new ArrayList<>();

        // note: entry below are for adding extra space/comment b/w enum entries
        //    this solution is kludgy.  Will reconsider better soln if this becomes more important.
        enumInfoList.add(new EnumInfo( "// Basic Fields"));  // comment line
        enumInfoList.addAll( generateEnumList(basicFieldList) );


        // note: 2 list entries below are for adding extra space/comment b/w enum entries
        //    this solution is kludgy.  Will reconsider better soln if this becomes more important.
        enumInfoList.add(new EnumInfo( ""));  // empty space line
        enumInfoList.add(new EnumInfo( "// Premium Fields"));  // comment line
        enumInfoList.addAll( generateEnumList(premiumFieldList) );

        return enumInfoList;
    }


    private List<EnumInfo> generateEnumList(List<ScreenerFieldDefinition> fieldList)
    {
        List<EnumInfo> resultList = new ArrayList<>();

        for (ScreenerFieldDefinition field : fieldList) {
            String fieldId = field.getFieldId();
            String sortable = field.getSortable().toString();
            String displayName = field.getDisplayName();
            String isPremium = field.getIsPremium().toString();

            String shortFieldId = fieldId;
            int dotIndex = shortFieldId.indexOf('.');
            if (dotIndex > 0) {
                shortFieldId = shortFieldId.substring(0, dotIndex);
            }

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(shortFieldId) );
            enumInfo.addParamValue(fieldId);
            enumInfo.addParamValue(sortable, false);
            enumInfo.addParamValue(displayName);
            enumInfo.addParamValue(isPremium, false);
            resultList.add(enumInfo);
        }

        return resultList;
    }



/*
FULL CATEGzORY LIST

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
