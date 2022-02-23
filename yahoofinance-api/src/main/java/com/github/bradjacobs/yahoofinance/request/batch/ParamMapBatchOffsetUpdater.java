/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;

import org.apache.commons.collections4.MapUtils;

import java.util.LinkedHashMap;
import java.util.Map;

public class ParamMapBatchOffsetUpdater implements ParamMapBatchUpdater
{
    private final String offsetKeyName;
    private final int batchSize;

    public ParamMapBatchOffsetUpdater(String offsetKeyName, int batchSize) {
        this.offsetKeyName = offsetKeyName;
        this.batchSize = batchSize;
    }

    @Override
    public Map<String, String> convert(Map<String, String> paramMap, int batchNumber) {
        if (MapUtils.isEmpty(paramMap)) {
            throw new IllegalArgumentException("Must supply a populated paramMap.");
        }

        paramMap = new LinkedHashMap<>(paramMap);
        if (! paramMap.containsKey(offsetKeyName)) {
            throw new IllegalStateException("error");  // todo - handle this better
        }

        int offsetShift = (batchNumber - 1) * batchSize;
        paramMap.put(offsetKeyName, String.valueOf(offsetShift));
        return paramMap;
    }
}
