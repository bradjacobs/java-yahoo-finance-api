/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
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

    // comment out lines below accordingly if only want to run individual ones.
    private static final List<EnumStringBlobGenerator> enumGeneratorList = Arrays.asList(
            new IntervalEnumGenerator(),
            new RangeEnumGenerator(),
            new RegionEnumGenerator(),
            new TypesEnumGenerator(),
            new ScreenerFieldEnumGenerator(),
            new SectorEnumGenerator(),
            new IndustryEnumGenerator(),
            new IpoEventFieldEnumGenerator(),
            new EarningsEventFieldEnumGenerator()
    );



    public static void main(String[] args)
    {
        for (EnumStringBlobGenerator enumGenerator : enumGeneratorList) {

            String destinationFileName = enumGenerator.getOutputClassName() + ".java";

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
