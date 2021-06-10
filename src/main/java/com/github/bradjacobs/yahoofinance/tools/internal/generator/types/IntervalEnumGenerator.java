/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static com.github.bradjacobs.yahoofinance.tools.internal.generator.types.TimeIntervalEnumInfoGenerator.generateEnumInfo;


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
    protected String fetchJson() throws IOException
    {
        OkHttpClient client = new OkHttpClient();
        Request request = new Request.Builder().url(URL).build();
        return  client.newCall(request).execute().body().string();
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
            enumInfoList.add( generateEnumInfo(value) );
        }

        return enumInfoList;
    }
}