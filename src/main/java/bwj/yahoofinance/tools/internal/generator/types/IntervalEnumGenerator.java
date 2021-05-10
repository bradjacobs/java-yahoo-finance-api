package bwj.yahoofinance.tools.internal.generator.types;

import bwj.yahoofinance.util.JsonDataExtractor;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static bwj.yahoofinance.tools.internal.generator.types.TimeIntervalEnumInfoGenerator.generateEnumInfo;


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
        JsonDataExtractor jsonDataExtractor = new JsonDataExtractor(json);

        List<String> descriptionValues = jsonDataExtractor.findStringValues("/", "description");
        if (descriptionValues.isEmpty()) {
            return Collections.emptyList();
        }

        String description = descriptionValues.get(0);
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