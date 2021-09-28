package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;


//  https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats,financials,valuation,sector_industry,esgscores,income,cashflowstatement,balance_sheet,earnings,dividends_and_splits,profile,fair_value,popular_filters,changes_in_price_and_market_cap,changes_in_volume_and_ownership,valuation_metric,profitability_ratios_and_dividends,debt_ratios,liquidity_ratios,eps_and_income_statement,cash_flow,esg_scores,short_interest,fair_value
//  &dependentvalues=Utilities
//  &dependentfield=sector&corsDomain=finance.yahoo.com

import com.jayway.jsonpath.JsonPath;
import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class IndustryEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "industry_template.txt";
    private static final String URL = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=sector_industry";

    private static final String EXTRA_URL_REQUEST_TEMPLATE = URL + "&dependentfield=sector&dependentvalues=%s";

    private static final String SECTOR_NAMES_JSON_PATH = "$.finance.result[0].fields.sector.labels[*].displayName";
    private static final String INDUSTRY_NAMES_JSON_PATH = "$.finance.result[0].fields.industry.labels[*].displayName";


    private final OkHttpClient client = new OkHttpClient();

    @Override
    protected String getTemplateFileName() {
        return TEMPLATE_NAME;
    }

    @Override
    protected String getUrl() {
        return URL;
    }


    /*
        the passed in JSON is only the first  (this method will make additional http requests).
     */
    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        // use the 'original' json response to get all the sectors.
        Map<String,String> sectorEnumNameDisplayNameMap = getEnumDisplayNameMap(json, SECTOR_NAMES_JSON_PATH);

        List<EnumInfo> enumInfoList = new ArrayList<>();

        // now do separate requests in order to figure out which industries map to which sectors.
        //
        for (Map.Entry<String, String> sectorEntry : sectorEnumNameDisplayNameMap.entrySet())
        {
            String sectorEnumName = sectorEntry.getKey();
            String sectorValue = sectorEntry.getValue();

            // note: 2 list entries below are for adding extra space/comment b/w enum entries
            //    this soln is kludgy.  Will reconsider better soln if this becomes more important.
            enumInfoList.add(new EnumInfo( ""));  // empty space line
            enumInfoList.add(new EnumInfo( "// " + sectorValue));  // comment line

            String sectorIndustryJson = executeSectorIndustryRequest(sectorValue);

            Map<String,String> industryEnumNameDisplayNameMap = getEnumDisplayNameMap(sectorIndustryJson, INDUSTRY_NAMES_JSON_PATH);

            for (Map.Entry<String, String> industryEntry : industryEnumNameDisplayNameMap.entrySet())
            {
                String industryEnumName = industryEntry.getKey();
                String industryValue = industryEntry.getValue();

                EnumInfo enumInfo = new EnumInfo( industryEnumName );
                enumInfo.addParamValue(industryValue);
                enumInfo.addParamValue(sectorEnumName, false);
                enumInfoList.add(enumInfo);
            }
        }

        return enumInfoList;
    }

    private String executeSectorIndustryRequest(String sectorName)
    {
        sectorName = sectorName.replace(" ", "%20");

        String industryUrl = String.format(EXTRA_URL_REQUEST_TEMPLATE, sectorName);
        Request request = new Request.Builder().url(industryUrl).build();
        try {
            return client.newCall(request).execute().body().string();
        }
        catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private Map<String,String> getEnumDisplayNameMap(String json, String path)
    {
        Map<String,String> resultMap = new TreeMap<>(); // <-- sort via TreeMap

        List<String> displayNames = JsonPath.read(json, path);
        for (String displayName : displayNames) {
            String enumName = EnumInfo.makeEnumStyleFriendly(displayName);
            resultMap.put(enumName, displayName);
        }

        return resultMap;
    }
}
