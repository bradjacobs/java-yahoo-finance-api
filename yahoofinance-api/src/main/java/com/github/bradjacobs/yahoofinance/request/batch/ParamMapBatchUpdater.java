/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;


import java.util.Map;

public interface ParamMapBatchUpdater
{
    Map<String,String> convert(Map<String,String> paramMap, int batchNumber);

}
