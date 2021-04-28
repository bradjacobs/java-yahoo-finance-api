/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.model.request;


import bwj.yahoofinance.YahooEndpoint;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class YahooFinanceRequest
{
    private String ticker;
    private Set<YahooEndpoint> endpoints = new LinkedHashSet<>();
    protected Map<String,String> paramMap = new LinkedHashMap<>();

    public YahooFinanceRequest() { }

    public YahooFinanceRequest(String ticker) {
        this.ticker = ticker;
    }

    public YahooFinanceRequest(String ticker, YahooEndpoint endpoint) {
        this.ticker = ticker;
        this.endpoints.add(endpoint);
    }

    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }


    public void addEndpoint(YahooEndpoint... endpoints) {
        if (endpoints != null) {
            this.endpoints.addAll(Arrays.asList(endpoints));
        }
    }

    public Set<YahooEndpoint> getEndpoints() {
        return Collections.unmodifiableSet(this.endpoints);
    }

    public Map<String, String> getParamMap() {

        return Collections.unmodifiableMap(this.paramMap);
    }

    public void addParams(Map<String, String> paramMap) {
        this.paramMap.putAll(paramMap);
    }

    public void addParam(String key, String value) {
        this.paramMap.put(key, value);
    }



    // todo: ponder this one
    public YahooEndpoint getEndpoint() {
        if (!this.endpoints.isEmpty()) {
            return this.endpoints.iterator().next();
        }
        return null;
    }


    public static class Builder {

        // use collection to allow for case where some endpoints allow multiple ticker values
        private Set<String> tickers = new LinkedHashSet<>();  // preserve insertion order

        private Set<YahooEndpoint> endpoints = new LinkedHashSet<>();
        private Map<String,String> paramMap = new LinkedHashMap<>();

        public Builder() { }

        public Builder withTicker(String... tickers) {
            if (tickers != null) {
                this.tickers.addAll(Arrays.asList(tickers));
            }
            return this;
        }

        public Builder withEndpoint(YahooEndpoint... endpoints) {
            if (endpoints != null) {
                this.endpoints.addAll(Arrays.asList(endpoints));
            }
            return this;
        }

        public Builder addParameter(String key, String value) {
            this.paramMap.put(key, value);
            return this;
        }

        public YahooFinanceRequest build() {
            YahooFinanceRequest req = new YahooFinanceRequest();
            req.addEndpoint(this.endpoints.toArray(new YahooEndpoint[0]));

            if (this.tickers.size() > 0) {
                YahooEndpoint endpoint = req.getEndpoint();
                if (endpoint != null && endpoint.isSupportsMultipleTickers()) {
                    req.setTicker(String.join(",", this.tickers));
                }
                else {
                    req.setTicker(this.tickers.iterator().next());
                }
            }
            else {
                req.setTicker("");
            }

            req.addParams(paramMap);
            return req;
        }
    }

}