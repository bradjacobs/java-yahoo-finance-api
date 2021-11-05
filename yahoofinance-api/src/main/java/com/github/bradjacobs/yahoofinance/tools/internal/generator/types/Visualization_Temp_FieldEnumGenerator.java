/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.JsonPath;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.*;
import java.util.stream.Collectors;

public class Visualization_Temp_FieldEnumGenerator extends EnumStringBlobGenerator
{
    private static final String TEMPLATE_NAME = "visualization_field_template.txt";

    private static final JsonMapper mapper = JsonMapperFactory.getPrettyMapper();

    private String readFile(String fileName)
    {
        String dir = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-api/src/test/resources/visualfields/";
        String fullPath = dir + fileName;
        try {
            return FileUtils.readFileToString(new File(fullPath), Charset.defaultCharset());
        } catch (IOException e) {
            throw new RuntimeException("Cant read file!");
        }
    }


    @Override
    protected String getTemplateFileName()
    {
        return TEMPLATE_NAME;
    }

    @Override
    protected String fetchJson() throws IOException
    {
        String econEventJson = readFile("economic_event.json");
        String researchReportsJson = readFile("research_reports.json");

        return researchReportsJson;
    }


    @Override
    protected String getUrl() {
        return "";
    }


    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        String earningsJson = readFile("earnings.json");
        String econEventJson = readFile("economic_event.json");
        String ipoInfoJson = readFile("ipo_info.json");
        String researchReportsJson = readFile("research_reports.json");
        String splitsJson = readFile("splits.json");

        Map<String, EnumInfo> earnMap = getSubSectionEnumList(earningsJson);
        Map<String, EnumInfo> econMap = getSubSectionEnumList(econEventJson);
        Map<String, EnumInfo> ipoMap = getSubSectionEnumList(ipoInfoJson);
        Map<String, EnumInfo> researcMap = getSubSectionEnumList(researchReportsJson);
        Map<String, EnumInfo> splitsMap = getSubSectionEnumList(splitsJson);

        Map<String, EnumInfo> commonMap = new HashMap<>();

        List<Map<String, EnumInfo>> listOfMaps = new ArrayList<>();
        listOfMaps.add(earnMap);
        listOfMaps.add(econMap);
        listOfMaps.add(ipoMap);
        listOfMaps.add(researcMap);
        listOfMaps.add(splitsMap);

        Set<String> commonEnumNames = new HashSet<>();
        Set<String> enumNames = new HashSet<>();

        for (Map<String, EnumInfo> listOfMap : listOfMaps) {
            Set<Map.Entry<String, EnumInfo>> mapEntries = listOfMap.entrySet();
            for (Map.Entry<String, EnumInfo> mapEntry : mapEntries) {
                String enumName = mapEntry.getKey();
                EnumInfo enumInfo = mapEntry.getValue();

                if (enumNames.contains(enumName)) {
                    commonEnumNames.add(enumName);
                    if (! commonMap.containsKey(enumName)) {
                        commonMap.put(enumName, enumInfo);
                    }
                }
                enumNames.add(enumName);
            }
        }

        for (String commonEnumName : commonEnumNames) {
            for (Map<String, EnumInfo> listOfMap : listOfMaps) {
                listOfMap.remove(commonEnumName);
            }
        }

        List<EnumInfo> masterList = new ArrayList<>();
        masterList.addAll(makeList("Common", commonMap.values()));
        masterList.addAll(makeList("Earnings", earnMap.values()));
        masterList.addAll(makeList("Econ", econMap.values()));
        masterList.addAll(makeList("IPO", ipoMap.values()));
        masterList.addAll(makeList("Splits", splitsMap.values()));
        masterList.addAll(makeList("Research", researcMap.values()));


        return masterList;
    }

    private List<EnumInfo> makeList(String sectionName, Collection<EnumInfo> inputCollection)
    {
        List<EnumInfo> resultList = new ArrayList<>(inputCollection);
        Collections.sort(resultList);
        resultList.add(0, new EnumInfo( "// " + sectionName));  // prepend comment line
        return resultList;
    }

    private Map<String,EnumInfo> makeMap(List<EnumInfo> enumList)
    {
        Map<String,EnumInfo> resultMap = new HashMap<>();
        for (EnumInfo enumInfo : enumList) {
            resultMap.put(enumInfo.getEnumName(), enumInfo);
        }
        return resultMap;
    }


    private Map<String,EnumInfo> getSubSectionEnumList(String json)
    {
        List<Map<String, Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].fields.*");
        ScreenerFieldDefinition[] fields = mapper.convertValue(listOfMaps, ScreenerFieldDefinition[].class);

        // filter out fields want to ignore.
        List<ScreenerFieldDefinition> basicFieldList = Arrays.stream(fields)
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> !sf.getIsPremium())
                .filter(sf -> sf.getCategory().getCategoryId().equalsIgnoreCase("visualizations"))
                .collect(Collectors.toList());

        List<EnumInfo> enumInfoList = new ArrayList<>();

        // note: entry below are for adding extra space/comment b/w enum entries
        //    this solution is kludgy.  Will reconsider better soln if this becomes more important.
        //enumInfoList.add(new EnumInfo( "// " + sectonName));  // comment line
        enumInfoList.addAll( generateEnumList(basicFieldList) );


        Map<String,EnumInfo> resultMap = new HashMap<>();
        for (EnumInfo enumInfo : enumInfoList) {
            resultMap.put(enumInfo.getEnumName(), enumInfo);
        }
        return resultMap;

    }


    private List<EnumInfo> generateEnumList(List<ScreenerFieldDefinition> fieldList)
    {
        List<EnumInfo> resultList = new ArrayList<>();

        for (ScreenerFieldDefinition field : fieldList) {
            String fieldId = field.getFieldId();
            String sortable = field.getSortable().toString();
            String displayName = field.getDisplayName();
            //String isPremium = field.getIsPremium().toString();

            String shortFieldId = fieldId;
            int dotIndex = shortFieldId.indexOf('.');
            if (dotIndex > 0) {
                shortFieldId = shortFieldId.substring(0, dotIndex);
            }

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(shortFieldId) );
            enumInfo.addParamValue(fieldId);
            enumInfo.addParamValue(sortable, false);
            enumInfo.addParamValue(displayName);
            //enumInfo.addParamValue(isPremium, false);
            resultList.add(enumInfo);
        }

        return resultList;
    }



}
