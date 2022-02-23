/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.collections4.MapUtils;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ParamMapSymbolBatchUpdater implements ParamMapBatchUpdater
{
    private final String symbolsKeyName;
    private final int batchSize;

    private static final String DELIM = ",";

    public ParamMapSymbolBatchUpdater(String offsetKeyName, int batchSize) {
        this.symbolsKeyName = offsetKeyName;
        this.batchSize = batchSize;
    }

    @Override
    public Map<String, String> convert(Map<String, String> paramMap, int batchNumber) {
        if (MapUtils.isEmpty(paramMap)) {
            throw new IllegalArgumentException("Must supply a populated paramMap.");
        }

        paramMap = new LinkedHashMap<>(paramMap);

        String symbolString = paramMap.get(symbolsKeyName);
        if (symbolString == null) {
            throw new IllegalStateException("error");  // todo - handle this better
        }

        List<String> symbolList = Arrays.asList(symbolString.split(DELIM));
        List<List<String>> symbolBatchLists = ListUtils.partition(symbolList, batchSize);

        if (batchNumber <= symbolBatchLists.size()) {
            List<String> batchList = symbolBatchLists.get(batchNumber -1);
            String batchSymbolString = String.join(DELIM,  batchList.toArray(new String[0]));
            paramMap.put(symbolsKeyName, batchSymbolString);
        }

        return paramMap;
    }
}
