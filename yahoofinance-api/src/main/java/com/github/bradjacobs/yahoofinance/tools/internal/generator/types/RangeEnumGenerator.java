/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.List;


class RangeEnumGenerator extends EnumStringBlobGenerator
{
    @Override
    protected String getUrl() {
        return "https://query1.finance.yahoo.com/v7/finance/chart/AAPL?range=1dx&interval=1d";
    }

    @Override
    protected String getOutputClassName() {
        return "Range";
    }

    @Override
    protected String getTemplateFileName() {
        return "single_value_template.txt";
    }

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<String> valueList = JsonPath.read(json, "$.chart.result[0].meta.validRanges");

        List<EnumInfo> enumInfoList = new ArrayList<>();
        for (String value : valueList)
        {
            enumInfoList.add( TimeIntervalEnumInfoGenerator.generateEnumInfo(value) );
        }
        return enumInfoList;
    }
}