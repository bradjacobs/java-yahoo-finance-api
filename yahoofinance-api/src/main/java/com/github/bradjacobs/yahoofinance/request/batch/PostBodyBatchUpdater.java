/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.batch;


public interface PostBodyBatchUpdater
{
    String convert(String postBody, int batchNumber);
}
