/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Simple class to read in fields stored in plain text file.
 *
 */
public class QuoteRequestFieldFactory
{
    private static final String FILE_NAME = "quote_fields.txt";
    private static final Map<Type, List<String>> TYPE_FIELD_MAP = new HashMap<>(); // (initialized below)

    private QuoteRequestFieldFactory() { }


    public static List<String> getQuoteFields(Type type)
    {
        if (type == null) {
            throw new IllegalArgumentException("Must provide a 'type");
        }

        List<String> fieldList = TYPE_FIELD_MAP.get(type);
        if (fieldList == null) {
            throw new UnsupportedOperationException("QuoteFields is currently unsupported for type: " + type);
        }

        return fieldList;
    }


    private static List<String> readAllFields()
    {
        String rawText = ResourceUtil.readResourceFileAsString(FILE_NAME);
        String[] lines = rawText.split("\n");
        List<String> dataList = Arrays.asList(lines);

        // in case the file had any 'blank' lines, remove them
        if (dataList.contains("")) {
            dataList = new ArrayList<>(dataList);
            dataList.removeAll(Collections.singleton(""));
        }
        return dataList;
    }


    // todo: don't know which fields map to types exactly, for now just focus on equity vs "non-equity"
    //  (list was derived from observations and could be (probably) incorrect/incomplete
    private static final List<String> NON_EQUITY_FIELDS = Arrays.asList(
        "circulatingSupply", "coinImageUrl", "contractSymbol", "expireDate", "expireIsoDate", "fromCurrency",
        "fromExchange", "lastMarket", "openInterest", "startDate", "strike", "toCurrency", "toExchange",
        "totalAssets", "trailingThreeMonthNavReturns", "trailingThreeMonthReturns", "volume24Hr",
        "volumeAllCurrencies", "ytdReturn"
    );

    static {
        // just going to load a map in memory with the choices (b/c it's pretty small)
        List<String> allFields = readAllFields();
        TYPE_FIELD_MAP.put(Type.ALL, Collections.unmodifiableList(allFields));

        List<String> equityFields = new ArrayList<>(allFields);
        equityFields.removeAll(NON_EQUITY_FIELDS);
        TYPE_FIELD_MAP.put(Type.EQUITY, Collections.unmodifiableList(equityFields));
    }
}
