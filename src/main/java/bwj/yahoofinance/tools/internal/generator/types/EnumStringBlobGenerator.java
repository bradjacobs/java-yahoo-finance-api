/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.tools.internal.generator.types;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

abstract class EnumStringBlobGenerator
{
    private static final String INDENT = "    ";
    private static final String NEW_LINE = System.lineSeparator();

    private static final String RESOURCES_TEMPLATE_DIR = "tools/internal/generator/types/templates/";
    private static final String TEMPLATE_PLACEHOLDER = "{ENUM_LIST}";
    private static final String GENERATOR_CLASS_PLACEHOLDER = "{GENERATOR_CLASS}";


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
        String template = readResourceTemplateFile(this.templateFilePath);
        String json = fetchJson();
        List<EnumInfo> enumInfoList = convertJsonToEnumInfo(json);
        String enumStringBlob = convertToEnumStringBlob(enumInfoList);

        String fileData = template.replace(TEMPLATE_PLACEHOLDER, enumStringBlob);

        //  tbd if this part is worthwhile
        //fileData = template.replace(GENERATOR_CLASS_PLACEHOLDER, this.getClass().getCanonicalName());
        //fileData = template.replace(GENERATOR_CLASS_PLACEHOLDER, this.getClass().getSimpleName());

        return fileData;
    }


    public String convertToEnumStringBlob(List<EnumInfo> enumInfoList)
    {
        StringBuilder sb = new StringBuilder();

        for (EnumInfo enumInfo : enumInfoList)
        {
            if (sb.length() > 0) {
                sb.append(",");
                sb.append(NEW_LINE);
            }

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
            }
        }

        // finish off with a semi;
        sb.append(";");
        sb.append(NEW_LINE);

        return sb.toString();
    }


    protected String readResourceTemplateFile(String filePath)
    {
        try {
            URL resource = getClass().getClassLoader().getResource(filePath);
            if (resource == null) {
                throw new RuntimeException(String.format("Unable to find resource file: %s", this.templateFilePath));
            }
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read resource file: %s.  Reason: %s", this.templateFilePath, e.getMessage()), e);
        }
    }

}
