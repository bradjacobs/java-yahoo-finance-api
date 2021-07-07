/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.jayway.jsonpath.JsonPath;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


class SectorEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "sector_template.txt";

    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=sector_industry";


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
        List<String> displayNames = JsonPath.read(json, "$.finance.result[0].fields.sector.labels[*].displayName");
        List<String> values = JsonPath.read(json, "$.finance.result[0].fields.sector.labels[*].criteria.operands[1]");

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