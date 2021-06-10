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
    protected String fetchJson() throws IOException
    {
        OkHttpClient client = new OkHttpClient();
        Request request = new Request.Builder().url(URL).build();
        return  client.newCall(request).execute().body().string();
    }

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<String> valueList = JsonPath.read(json, "$.chart.result[0].meta.validRanges");

        List<EnumInfo> enumInfoList = new ArrayList<>();
        for (String value : valueList)
        {
            enumInfoList.add( generateEnumInfo(value) );
        }
        return enumInfoList;
    }
}