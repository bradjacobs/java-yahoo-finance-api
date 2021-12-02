/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.YahooFieldDefinition;

import java.util.List;
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
    protected List<YahooFieldDefinition> filterFields(List<YahooFieldDefinition> fieldList)
    {
        return fieldList.stream()
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> !sf.getFieldId().equalsIgnoreCase("count"))  // skip count for now
                .collect(Collectors.toList());
    }
}
