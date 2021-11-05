package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

public class VisualizationEnumGenerator
{
    private static final JsonMapper mapper = JsonMapperFactory.getPrettyMapper();

    public static void main(String[] args) throws Exception {
        VisualizationEnumGenerator visualizationEnumGenerator = new VisualizationEnumGenerator();
        visualizationEnumGenerator.doIt();;
    }

    public void doIt() throws Exception
    {
        String earningsJson = readFile("earnings.json");
        String econEventJson = readFile("economic_event.json");
        String ipoInfoJson = readFile("ipo_info.json");
        String researchReportsJson = readFile("research_reports.json");

        String json = econEventJson;

        List<Map<String, Object>> listOfMaps = JsonPath.read(json, "$.finance.result[0].fields.*");
        ScreenerFieldDefinition[] fields = mapper.convertValue(listOfMaps, ScreenerFieldDefinition[].class);


        int kjjj = 333;


    }

    private String readFile(String fileName) throws IOException
    {
        String dir = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-api/src/test/resources/visualfields/";
        String fullPath = dir + fileName;

        return FileUtils.readFileToString(new File(fullPath), Charset.defaultCharset());
    }

}
