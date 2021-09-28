/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ScreenerFieldEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "screener_field_template.txt";
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats%2Cfinancials%2Cvaluation%2Csector_industry%2Cesgscores%2Cincome%2Ccashflowstatement%2Cbalance_sheet%2Cearnings%2Cdividends_and_splits%2Cprofile%2Cfair_value%2Cpopular_filters%2Cchanges_in_price_and_market_cap%2Cchanges_in_volume_and_ownership%2Cvaluation_metric%2Cprofitability_ratios_and_dividends%2Cdebt_ratios%2Cliquidity_ratios%2Ceps_and_income_statement%2Ccash_flow%2Cesg_scores%2Cshort_interest";

    private static final JsonMapper mapper = JsonMapperSingleton.getPrettyInstance();


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
        List<ScreenerFieldDefinition> basicFieldList = Arrays.stream(fields)
            .filter(sf -> !sf.getDeprecated())
            .filter(sf -> !sf.getIsPremium())
            .collect(Collectors.toList());

        List<ScreenerFieldDefinition> premiumFieldList = Arrays.stream(fields)
            .filter(sf -> !sf.getDeprecated())
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



}
