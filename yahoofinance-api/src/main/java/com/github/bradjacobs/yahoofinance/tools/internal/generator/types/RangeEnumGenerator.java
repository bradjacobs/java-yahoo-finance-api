/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.List;


class RangeEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "range_template.txt";

    private static final String URL = "https://query1.finance.yahoo.com/v7/finance/chart/AAPL?range=1dx&interval=1d";

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
        List<String> valueList = JsonPath.read(json, "$.chart.result[0].meta.validRanges");

        List<EnumInfo> enumInfoList = new ArrayList<>();
        for (String value : valueList)
        {
            enumInfoList.add( TimeIntervalEnumInfoGenerator.generateEnumInfo(value) );
        }
        return enumInfoList;
    }
}