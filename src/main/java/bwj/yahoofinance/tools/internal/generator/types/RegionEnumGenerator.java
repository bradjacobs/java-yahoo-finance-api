package bwj.yahoofinance.tools.internal.generator.types;

import bwj.yahoofinance.util.JsonDataExtractor;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

class RegionEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "region_template.txt";

    // grab avaialable regions from Yahoo's sereender defn fields.
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats";

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

        List<Map<String, Object>> listOfMaps = jsonDataExtractor.getListOfMaps("/finance/result/0/fields/region/labels");

        List<EnumInfo> enumInfoList = new ArrayList<>();

        for (Map<String, Object> entryMap : listOfMaps)
        {
            String displayName = (String) entryMap.get("displayName");
            Map<String, List<String>> criteria = (Map<String, List<String>>) entryMap.get("criteria");

            List<String> operandList = criteria.get("operands");
            String value = operandList.get(1);

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(displayName) );
            enumInfo.addParamValue(value.toUpperCase());
            enumInfo.addParamValue(displayName);
            enumInfoList.add(enumInfo);
        }

        Collections.sort(enumInfoList);
        return enumInfoList;
    }

}
