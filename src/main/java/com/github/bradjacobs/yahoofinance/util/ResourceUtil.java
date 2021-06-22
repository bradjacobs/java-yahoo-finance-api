package com.github.bradjacobs.yahoofinance.util;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class ResourceUtil
{
    private ResourceUtil() { }

    public static String readResourceFileAsString(String fileName)
    {
        if (StringUtils.isEmpty(fileName)) {
            throw new IllegalArgumentException("Must provide a resource fileName.");
        }

        try {
            try(  InputStream is = ResourceUtil.class.getClassLoader().getResourceAsStream(fileName);
                  BufferedInputStream bis = new BufferedInputStream(is) )
            {
                return IOUtils.toString(bis, StandardCharsets.UTF_8.name());
            }
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }
}
