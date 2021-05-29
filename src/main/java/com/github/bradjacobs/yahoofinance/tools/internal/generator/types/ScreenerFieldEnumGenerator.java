/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.Category;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.JsonPath;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class ScreenerFieldEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "screener_field_template.txt";
    private static final JsonMapper mapper = new JsonMapper();

    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats%2Cfinancials%2Cvaluation%2Csector_industry%2Cesgscores%2Cincome%2Ccashflowstatement%2Cbalance_sheet%2Cearnings%2Cdividends_and_splits%2Cprofile%2Cfair_value%2Cpopular_filters%2Cchanges_in_price_and_market_cap%2Cchanges_in_volume_and_ownership%2Cvaluation_metric%2Cprofitability_ratios_and_dividends%2Cdebt_ratios%2Cliquidity_ratios%2Ceps_and_income_statement%2Ccash_flow%2Cesg_scores%2Cshort_interest%2Cfair_value&corsDomain=finance.yahoo.com";

    @Override
    protected String getTemplateFileName()
    {
        return TEMPLATE_NAME;
    }

    @Override
    protected String fetchJson() throws IOException
    {
        OkHttpClient client = new OkHttpClient();
        Request request = new Request.Builder().url(URL).build();
        return  client.newCall(request).execute().body().string();
    }

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<Map<String, Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].fields.*");
        ScreenerFieldDefinition[] fields = mapper.convertValue(listOfMaps, ScreenerFieldDefinition[].class);

        // filter out fields want to ignore.
        List<ScreenerFieldDefinition> filteredList = Arrays.stream(fields)
            .filter(sf -> !sf.getDeprecated())
            .filter(sf -> !sf.getIsPremium())
            .collect(Collectors.toList());

        Map<Category, Set<ScreenerFieldDefinition>> categoryFieldMap = new TreeMap<>(); // treemap to keep key order consistent

        for (ScreenerFieldDefinition field : filteredList) {
            Category category = field.getCategory();
            Set<ScreenerFieldDefinition> categoryFields = categoryFieldMap.computeIfAbsent(category, k -> new TreeSet<>());
            categoryFields.add(field);
        }

        List<EnumInfo> enumInfoList = new ArrayList<>();

        for (ScreenerFieldDefinition field : filteredList) {
            String fieldId = field.getFieldId();
            String sortable = field.getSortable().toString();
            String displayName = field.getDisplayName();

            String shortFieldId = fieldId;
            int dotIndex = shortFieldId.indexOf('.');
            if (dotIndex > 0) {
                shortFieldId = shortFieldId.substring(0, dotIndex);
            }

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(shortFieldId) );
            enumInfo.addParamValue(fieldId);
            enumInfo.addParamValue(sortable, false);
            enumInfo.addParamValue(displayName);
            enumInfoList.add(enumInfo);
        }

        return enumInfoList;
    }


}
