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
    @Override
    protected String getUrl() {
        // grab available regions from Yahoo's screener definition fields.
        return "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats";
    }

    @Override
    protected String getOutputClassName() {
        return "Region";
    }

    @Override
    protected String getTemplateFileName() {
        return "region_template.txt";
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
