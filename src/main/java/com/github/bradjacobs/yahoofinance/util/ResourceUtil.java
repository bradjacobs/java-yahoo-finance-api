package com.github.bradjacobs.yahoofinance.util;

import org.apache.commons.lang3.StringUtils;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ResourceUtil
{
    private ResourceUtil() { }

    public static String readResourceFileAsString(String fileName)
    {
        if (StringUtils.isEmpty(fileName)) {
            throw new IllegalArgumentException("Must provide a resource fileName.");
        }

        try {
            URL resource = ResourceUtil.class.getClassLoader().getResource(fileName);
            if (resource == null) {
                throw new IllegalArgumentException("Unable to reade resourse: " + fileName);
            }
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }
}
