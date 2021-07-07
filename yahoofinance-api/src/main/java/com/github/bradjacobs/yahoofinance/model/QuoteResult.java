/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.model;

/**
 * Hold data for a Quote result entry
 */

/* NOTE: it looks like Quote & Screener both return the same result type,
        thus have a common abstract class.  Once discover that they deviate,
        then the abstract class is subject to removal.
 */
public class QuoteResult extends QueryResult
{

}
