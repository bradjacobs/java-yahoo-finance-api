/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen.YahooFieldDefinition;

import java.util.List;
import java.util.stream.Collectors;

// TODO - the output enum list appears to be "incomplete", but it's (currently)
//   unclear which values are (and aren't) supported
public class EarningsEventFieldEnumGenerator extends AbstractFieldEnumGenerator
{
    @Override
    protected String getUrl() {
        return "https://query1.finance.yahoo.com/v1/finance/screener/instrument/earnings/fields?lang=en-US&region=US";
    }

    @Override
    protected String getOutputClassName() {
        return "EarningsEventField";
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
                .filter(sf -> sf.getCategory() != null && sf.getCategory().getCategoryId().equalsIgnoreCase("visualizations"))
                .filter(sf -> !sf.getFieldId().equalsIgnoreCase("count"))  // skip count for now
                .collect(Collectors.toList());
    }
}
