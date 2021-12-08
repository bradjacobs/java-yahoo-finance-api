/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

// todo - refactor to combine redundant code from sector
public class StockValueDescriptionFieldGenerator extends EnumStringBlobGenerator
{
    @Override
    protected String getUrl() {
        return "https://query1.finance.yahoo.com/ws/screeners/v2/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=fair_value";
    }

    @Override
    protected String getOutputClassName() {
        return "StockValueDescription";
    }

    @Override
    protected String getTemplateFileName() {
        return "sector_template.txt";
    }

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<String> displayNames = JsonPath.read(json, "$.finance.result[0].fields.value_description.labels[*].displayName");
        List<String> values = JsonPath.read(json, "$.finance.result[0].fields.value_description.labels[*].criteria.operands[1]");

        List<EnumInfo> enumInfoList = new ArrayList<>();
        int elementCount = displayNames.size();

        for (int i = 0; i < elementCount; i++)
        {
            String displayName = displayNames.get(i);
            String value = values.get(i);

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(displayName) );
            enumInfo.addParamValue(value);
            enumInfoList.add(enumInfo);
        }

        Collections.sort(enumInfoList);
        return enumInfoList;
    }
}
