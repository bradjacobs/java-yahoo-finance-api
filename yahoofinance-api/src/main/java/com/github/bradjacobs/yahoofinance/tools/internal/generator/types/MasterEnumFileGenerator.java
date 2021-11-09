/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Internal tool that does web requests for data in order to generate some of the "enumeration / types" files.
 *
 * (bit clunky, but does the job)
 */
class MasterEnumFileGenerator
{
    // note:   of course this only works if the 'current directory' is the project root.
    private static final String DESTINATION_DIR = "./yahoofinance-api/src/main/java/com/github/bradjacobs/yahoofinance/types/";

    private static final List<EnumStringBlobGenerator> enumGenerators = new ArrayList<>();

    private static final Map<String,EnumStringBlobGenerator> enumGeneratorMap = new LinkedHashMap<>();

    // comment out lines below according if only want to run individual ones.
    static {
        enumGeneratorMap.put("Interval.java", new IntervalEnumGenerator());
        enumGeneratorMap.put("Range.java", new RangeEnumGenerator());
        enumGeneratorMap.put("Region.java", new RegionEnumGenerator());
        enumGeneratorMap.put("Type.java", new TypesEnumGenerator());
        enumGeneratorMap.put("ScreenerField.java", new ScreenerFieldEnumGenerator());
        //////enumGeneratorMap.put("Exchange.java", new ExchangeEnumGenerator());  // to be removed from auto-gen
        enumGeneratorMap.put("Sector.java", new SectorEnumGenerator());
        enumGeneratorMap.put("Industry.java", new IndustryEnumGenerator());
    }


    public static void main(String[] args)
    {
        for (Map.Entry<String, EnumStringBlobGenerator> entry : enumGeneratorMap.entrySet())
        {
            String destinationFileName = entry.getKey();
            EnumStringBlobGenerator enumGenerator = entry.getValue();

            String destinationFilePath = DESTINATION_DIR + destinationFileName;
            try {
                File destFile = new File(destinationFilePath);
                String fileData = enumGenerator.regenerateEnumFileBody();
                FileUtils.writeStringToFile(destFile, fileData, StandardCharsets.UTF_8);
            }
            catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
                e.printStackTrace();
            }
        }
    }

}
