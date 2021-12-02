/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Map;

public interface YahooBatchableRequest extends YahooRequest
{
    int getBatchSize();
    int getMaxResults();

    Map<String,String> getParamMap(int batchNumber);
    String getPostBody(int batchNumber);
}