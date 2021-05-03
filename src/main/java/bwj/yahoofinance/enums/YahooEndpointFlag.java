/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.enums;

public enum YahooEndpointFlag
{
    // can support multiple tickers on request  "...?symbols=AAPL,MSFT,..."
    FLAG_SUPPORT_MULTI_TICKERS,

    // is query endpoint  (might not be any ticker symbol on the request)
    FLAG_IS_QUERY,

    // requires ticker on url in form of ...?symbol=AAPL
    FLAG_REQUIRES_SYMBOL_PARAM,

    // endpoint requires a yahoo crumb
    FLAG_REQUIRES_CRUMB,

    // endpoint requires POST method (instead of GET)
    FLAG_REQUIRES_POST,
}
