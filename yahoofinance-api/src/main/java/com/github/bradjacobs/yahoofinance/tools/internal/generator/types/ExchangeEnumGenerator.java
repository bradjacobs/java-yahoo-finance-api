/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;


class ExchangeEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "exchange_template.txt";
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=profile";


    // apparently legal exchange value, even though it's not returned from the query.  (add explicitly)
    private static final List<String> EXTRA_EXCHANGES = Arrays.asList("ASE", "PNK");


    @Override
    protected String getTemplateFileName() {
        return TEMPLATE_NAME;
    }

    @Override
    protected String getUrl() {
        return URL;
    }

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<String> displayNames = JsonPath.read(json, "$.finance.result[0].fields.exchange.labels[*].displayName");
        List<String> codeNames = JsonPath.read(json, "$.finance.result[0].fields.exchange.labels[*].criteria.operands[1]");

        List<EnumInfo> enumInfoList = new ArrayList<>();

        int elementCount = displayNames.size();

        for (int i = 0; i < elementCount; i++)
        {
            String displayName = displayNames.get(i);
            String codeName = codeNames.get(i);

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(displayName) );
            enumInfo.addParamValue(codeName);
            enumInfoList.add(enumInfo);
        }

        for (String extraExchange : EXTRA_EXCHANGES)
        {
            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(extraExchange) );
            enumInfo.addParamValue(extraExchange);
            enumInfoList.add(enumInfo);
        }

        Collections.sort(enumInfoList);
        return enumInfoList;
    }
}