/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.JsonPath;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 */
// TODO - the enums really has too much clutter.  It should be reorganized and/or pruned.
abstract public class AbstractFieldEnumGenerator extends EnumStringBlobGenerator
{
    private static final JsonMapper mapper = JsonMapperFactory.getPrettyMapper();

    protected List<ScreenerFieldDefinition> convertToFieldDefinitions(String json)
    {
        List<Map<String, Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].fields.*");
        ScreenerFieldDefinition[] fields = mapper.convertValue(listOfMaps, ScreenerFieldDefinition[].class);
        return Arrays.asList(fields);
    }

    abstract protected List<ScreenerFieldDefinition> filterFields(List<ScreenerFieldDefinition> fieldList);

    @Override
    protected List<EnumInfo> convertJsonToEnumInfo(String json)
    {
        List<ScreenerFieldDefinition> fieldList = convertToFieldDefinitions(json);

        // filter out fields want to ignore.
        List<ScreenerFieldDefinition> filteredList = filterFields(fieldList);

        // after initial filtering, make 2 lists non-premium and premium
        List<ScreenerFieldDefinition> basicFieldList = filteredList.stream()
            .filter(sf -> !sf.getIsPremium())
            .collect(Collectors.toList());

        List<ScreenerFieldDefinition> premiumFieldList = filteredList.stream()
            .filter(ScreenerFieldDefinition::getIsPremium)
            .collect(Collectors.toList());

        boolean havePremiumFields = !premiumFieldList.isEmpty();

        List<EnumInfo> enumInfoList = new ArrayList<>();

        // note: entry below are for adding extra space/comment b/w enum entries
        //    this solution is kludgy.  Will reconsider better soln if this becomes more important.
        if (havePremiumFields) {
            enumInfoList.add(new EnumInfo( "// Basic Fields"));  // comment line
        }
        enumInfoList.addAll( generateEnumList(basicFieldList) );

        // note: 2 list entries below are for adding extra space/comment b/w enum entries
        //    this solution is kludgy.  Will reconsider better soln if this becomes more important.
        if (havePremiumFields) {
            enumInfoList.add(new EnumInfo( ""));  // empty space line
            enumInfoList.add(new EnumInfo( "// Premium Fields"));  // comment line
            enumInfoList.addAll( generateEnumList(premiumFieldList) );
        }
        return enumInfoList;
    }

    protected List<EnumInfo> generateEnumList(List<ScreenerFieldDefinition> fieldList)
    {
        List<EnumInfo> resultList = new ArrayList<>();

        for (ScreenerFieldDefinition field : fieldList) {
            String fieldId = field.getFieldId();
            String sortable = field.getSortable().toString();
            String displayName = field.getDisplayName();
            String isPremium = field.getIsPremium().toString();

            String shortFieldId = fieldId;
            int dotIndex = shortFieldId.indexOf('.');
            if (dotIndex > 0) {
                shortFieldId = shortFieldId.substring(0, dotIndex);
            }

            EnumInfo enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(shortFieldId) );
            enumInfo.addParamValue(fieldId);
            enumInfo.addParamValue(sortable, false);
            enumInfo.addParamValue(displayName);
            enumInfo.addParamValue(isPremium, false);
            resultList.add(enumInfo);
        }

        return resultList;
    }
}
