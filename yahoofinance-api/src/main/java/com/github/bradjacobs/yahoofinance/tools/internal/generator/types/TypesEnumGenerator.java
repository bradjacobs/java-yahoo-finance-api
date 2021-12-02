/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.List;

class TypesEnumGenerator extends EnumStringBlobGenerator
{
    @Override
    protected String getUrl() {
        // NOTE: this is a bad request _ON PURPOSE_.
        //   the error response has the required info
        return "https://query1.finance.yahoo.com/v1/finance/lookup?query=ABCD&type=FAKE";
    }

    @Override
    protected String getOutputClassName() {
        return "Type";
    }

    @Override
    protected String getTemplateFileName() {
        return "type_template.txt";
    }


    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        String description = JsonPath.read(json, "$.finance.error.description");
        int startBracketIndex = description.indexOf('[');
        int endBracketIndex = description.lastIndexOf(']');
        String intervalChoiceString = description.substring(startBracketIndex+1, endBracketIndex);

        String[] fieldValues = intervalChoiceString.split(", ");
        List<EnumInfo> enumInfoList = new ArrayList<>();
        for (String value : fieldValues)
        {
            enumInfoList.add( new EnumInfo( value.toUpperCase() ));
        }

        // extra discovered that "ALL" is also a valid choice (don't remember where)
        enumInfoList.add( new EnumInfo("ALL") );

        return enumInfoList;
    }
}