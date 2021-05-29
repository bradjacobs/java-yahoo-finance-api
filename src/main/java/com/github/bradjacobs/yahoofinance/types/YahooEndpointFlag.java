/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

public enum YahooEndpointFlag
{
    // can support multiple tickers on request  "...?symbols=AAPL,MSFT,..."
    FLAG_SUPPORT_MULTI_TICKERS,

    // is query endpoint  (might not be any ticker symbol on the request)
    FLAG_IS_QUERY,

    // endpoint that queires for general region information
    //  (i.e.  not data for tickers and not a query)
    FLAG_IS_REGION,

    // requires ticker on url in form of ...?symbol=AAPL
    FLAG_REQUIRES_SYMBOL_PARAM,

    // endpoint requires a yahoo crumb
    FLAG_REQUIRES_CRUMB,

    // endpoint requires POST method (instead of GET)
    FLAG_REQUIRES_POST,
}
