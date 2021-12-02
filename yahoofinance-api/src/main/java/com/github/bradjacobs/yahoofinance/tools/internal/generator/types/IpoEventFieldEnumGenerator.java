/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.ScreenerFieldDefinition;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class IpoEventFieldEnumGenerator extends AbstractFieldEnumGenerator
{
    @Override
    protected String getUrl() {
        return "https://query1.finance.yahoo.com/v1/finance/screener/instrument/ipo_info/fields?lang=en-US&region=US";
    }

    @Override
    protected String getOutputClassName() {
        return "IpoEventField";
    }

    @Override
    protected String getTemplateFileName() {
        return "field_template.txt";
    }

    @Override
    protected List<ScreenerFieldDefinition> filterFields(List<ScreenerFieldDefinition> fieldList)
    {
        return fieldList.stream()
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> !sf.getFieldId().equalsIgnoreCase("count"))  // skip count for now
                .collect(Collectors.toList());
    }
}
