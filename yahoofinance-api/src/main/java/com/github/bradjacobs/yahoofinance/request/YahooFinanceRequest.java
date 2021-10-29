/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;

import java.util.Collections;
import java.util.Map;

public class YahooFinanceRequest
{
    protected final YahooEndpoint endpoint;
    protected final String ticker;
    protected final Map<String,String> paramMap;
    protected final Object postBody;
    protected final Map<String,String> headerMap;

    private static final JsonMapper mapper = JsonMapperFactory.getMapper();


    public YahooFinanceRequest(YahooEndpoint endpoint, String ticker, Map<String,String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        this.endpoint = endpoint;
        this.ticker = ticker;
        this.paramMap = paramMap;
        this.postBody = postBody;
        this.headerMap = headerMap;
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

    public boolean isPost() {
        return this.endpoint != null && this.endpoint.isPostRequest();
    }

    public Object getPostObject()
    {
        return postBody;
    }

    public String getPostBody() {
        if (postBody == null) {
            return null;
        }
        else if (postBody instanceof String) {
            return (String)postBody;
        }
        else {
            // todo - this code belongs in different spot
            try {
                return mapper.writeValueAsString(postBody);
            }
            catch (JsonProcessingException e) {
                throw new InternalError("Unable to create request postBody: " + e.getMessage(), e);
            }
        }
    }
}
