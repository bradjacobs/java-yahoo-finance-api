/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.List;



class IntervalEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "interval_template.txt";

    // NOTE: this is a bad request _ON PURPOSE_.
    //   the error response has the required info
    private static final String URL = "https://query1.finance.yahoo.com/v7/finance/chart/AAPL?range=1d&interval=FAKE";


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
        String description = JsonPath.read(json, "$.chart.error.description");
        int startBracketIndex = description.indexOf('[');
        int endBracketIndex = description.lastIndexOf(']');
        String intervalChoiceString = description.substring(startBracketIndex+1, endBracketIndex);

        String[] fieldValues = intervalChoiceString.split(", ");
        List<EnumInfo> enumInfoList = new ArrayList<>();
        for (String value : fieldValues)
        {
            enumInfoList.add( TimeIntervalEnumInfoGenerator.generateEnumInfo(value) );
        }

        return enumInfoList;
    }
}