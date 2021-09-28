/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class RegionEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "region_template.txt";

    // grab available regions from Yahoo's screener definition fields.
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats";

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
        List<String> displayNameList = JsonPath.read(json, "$.finance.result[0].fields.region.labels[*].displayName");
        List<String> criteriaValueList = JsonPath.read(json, "$.finance.result[0].fields.region.labels[*].criteria.operands[1]");

        List<EnumInfo> enumInfoList = new ArrayList<>();

        for (int i = 0; i < displayNameList.size(); i++)
        {
            String displayName = displayNameList.get(i);
            String value = criteriaValueList.get(i);

            EnumInfo enumInfo = new EnumInfo(EnumInfo.makeEnumStyleFriendly(displayName));
            enumInfo.addParamValue(value.toUpperCase());
            enumInfo.addParamValue(displayName);
            enumInfoList.add(enumInfo);
        }

        Collections.sort(enumInfoList);
        return enumInfoList;
    }
}
