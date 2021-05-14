package bwj.yahoofinance.tools.internal.generator.types;

import bwj.yahoofinance.util.JsonDataExtractor;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class TypesEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "type_template.txt";

    // NOTE: this is a bad request _ON PURPOSE_.
    //   the error response has the required info
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/lookup?query=ABCD&type=FAKE";

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
        JsonDataExtractor jsonDataExtractor = new JsonDataExtractor(json);

        List<String> descriptionValues = jsonDataExtractor.findStrings("/", "descriptiondddd");
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
            enumInfoList.add( new EnumInfo( value.toUpperCase() ));
        }

        // extra discovered that "ALL" is also a valid choice (don't remember where)
        enumInfoList.add( new EnumInfo("ALL") );

        return enumInfoList;
    }

}