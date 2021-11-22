/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;


import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonConverter;

import java.util.Collections;
import java.util.Map;

public class YahooFinanceRequest implements YahooRequest
{
    protected final YahooEndpoint endpoint;
    protected final String ticker;
    protected final Map<String,String> paramMap;
    protected final Object postBodyObject;
    protected final Map<String,String> headerMap;

    public YahooFinanceRequest(YahooEndpoint endpoint, String ticker, Map<String,String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        this.endpoint = endpoint;
        this.ticker = ticker;
        this.paramMap = paramMap;
        this.postBodyObject = postBody;
        this.headerMap = headerMap;
    }

    @Override
    public YahooEndpoint getEndpoint() {
        return endpoint;
    }

    public String getTicker() {
        return ticker;
    }

    public Map<String, String> getParamMap() {
        return Collections.unmodifiableMap(this.paramMap);
    }

    @Override
    public Map<String, String> getHeaderMap() {
        if (this.headerMap == null) {
            return Collections.emptyMap();
        }
        return Collections.unmodifiableMap(this.headerMap);
    }

    public String getParam(String key) {
        if (key == null) { return null; }
        return this.paramMap.get(key);
    }

    public boolean isCrumbRequired() {
        return this.endpoint != null && this.endpoint.isCrumbRequest();
    }

    public Object getPostObject()
    {
        if (this.endpoint != null && this.endpoint.isPostRequest()) {
            return postBodyObject;
        }
        return null;
    }

    public String getPostBody() {
        return JsonConverter.toJson(getPostObject());
    }
}
