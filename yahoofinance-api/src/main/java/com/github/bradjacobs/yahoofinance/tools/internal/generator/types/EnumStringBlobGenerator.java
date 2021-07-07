/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.github.bradjacobs.yahoofinance.util.ResourceUtil;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

abstract class EnumStringBlobGenerator
{
    private static final String INDENT = "    ";
    private static final String NEW_LINE = System.lineSeparator();

    private static final String RESOURCES_TEMPLATE_DIR = "tools/internal/generator/types/templates/";
    private static final String TEMPLATE_PLACEHOLDER = "{ENUM_LIST}";
    private static final String GENERATOR_LINE_PLACEHOLDER = "{GENERATOR_LINE}";

    private static final String GENERATOR_LINE_TEMPLATE = "@Generated(value=\"yahoo-finance-api-internal-tools\", date=\"%s\")";

    private final String templateFilePath;

    public EnumStringBlobGenerator()
    {
        this.templateFilePath = RESOURCES_TEMPLATE_DIR + getTemplateFileName();
    }

    protected abstract String getTemplateFileName();
    protected abstract String fetchJson() throws IOException;
    protected abstract List<EnumInfo> convertJsonToEnumInfo(String json);


    public String regenerateEnumFileBody() throws IOException
    {
        String template = ResourceUtil.readResourceFileAsString(this.templateFilePath);
        String json = fetchJson();
        List<EnumInfo> enumInfoList = convertJsonToEnumInfo(json);
        String enumStringBlob = convertToEnumStringBlob(enumInfoList);

        String fileData = template.replace(TEMPLATE_PLACEHOLDER, enumStringBlob);
        fileData = fileData.replace(GENERATOR_LINE_PLACEHOLDER, String.format(GENERATOR_LINE_TEMPLATE, LocalDate.now().toString()));

        return fileData;
    }


    public String convertToEnumStringBlob(List<EnumInfo> enumInfoList)
    {
        StringBuilder sb = new StringBuilder();

        EnumInfo lastEnumInfo = enumInfoList.get(enumInfoList.size() - 1);


        for (EnumInfo enumInfo : enumInfoList)
        {
            sb.append(INDENT);
            sb.append(enumInfo.getEnumName());

            List<EnumInfo.EnumParamInfo> enumParamValues = enumInfo.getEnumParamValues();

            if (enumParamValues.size() > 0)
            {
                List<String> quotedParamValues = new ArrayList<>();
                for (EnumInfo.EnumParamInfo enumParamValue : enumParamValues)
                {
                    String value = enumParamValue.getParamValue();
                    if (enumParamValue.isString()) {
                        quotedParamValues.add("\"" + value + "\"");
                    }
                    else {
                        quotedParamValues.add(value);
                    }
                }
                sb.append("(");
                sb.append( String.join(", ", quotedParamValues) );
                sb.append(")");

                if (enumInfo.equals(lastEnumInfo)) {
                    sb.append(";");
                }
                else {
                    sb.append(",");
                }
            }
            sb.append(NEW_LINE);
        }

        // finish off with a semi;
        sb.append(NEW_LINE);

        return sb.toString();
    }


}
