/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;

import com.github.bradjacobs.yahoofinance.util.JsonConverter;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

public class CriteriaPostBodyBatchUpdater implements PostBodyBatchUpdater
{
    private static final String OFFSET_KEY_NAME = "offset";  // name of member from AbstractRequestCriteria
    private final int batchSize;

    public CriteriaPostBodyBatchUpdater(int batchSize) {
        this.batchSize = batchSize;
    }

    @Override
    public String convert(String postBody, int batchNumber) {
        if (StringUtils.isNotEmpty(postBody) && batchNumber > 1) {
            Map<String, Object> dataMap = JsonConverter.toMapOfObjects(postBody);
            Object existingValue = dataMap.get(OFFSET_KEY_NAME);
            if (existingValue == null) {
                throw new IllegalStateException("error");  // todo - handle this better
            }

            int offsetShift = (batchNumber - 1) * batchSize;
            if (existingValue instanceof Number) {
                dataMap.put(OFFSET_KEY_NAME, offsetShift);
            }
            else {
                dataMap.put(OFFSET_KEY_NAME, String.valueOf(offsetShift));
            }
            postBody = JsonConverter.toJson(dataMap);
        }
        return postBody;
    }
}
