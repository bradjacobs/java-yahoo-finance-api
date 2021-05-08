/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request.builder;


import bwj.yahoofinance.types.YahooEndpoint;

import java.util.Collections;
import java.util.Map;

public class YahooFinanceRequest
{
    protected final YahooEndpoint endpoint;
    protected final String ticker;
    protected final Map<String,String> paramMap;
    protected final Object postBody;


    protected YahooFinanceRequest(YahooEndpoint endpoint, String ticker, Map<String,String> paramMap)
    {
        this(endpoint, ticker, paramMap, null);
    }

    protected YahooFinanceRequest(YahooEndpoint endpoint, String ticker, Map<String,String> paramMap, Object postBody)
    {
        this.endpoint = endpoint;
        this.ticker = ticker;
        this.paramMap = paramMap;
        this.postBody = postBody;
    }


    public String getTicker() {
        return ticker;
    }

    public YahooEndpoint getEndpoint() {
        return endpoint;
    }

    public Map<String, String> getParamMap() {
        return Collections.unmodifiableMap(this.paramMap);
    }

    public String getParam(String key) {
        if (key == null) { return null; }
        return this.paramMap.get(key);
    }

}
