/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.model.request;


import bwj.yahoofinance.YahooEndpoint;
import bwj.yahoofinance.YahooModule;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class YahooFinanceRequest
{
    private String ticker;
    private YahooEndpoint endpoint;
    protected Map<String,String> paramMap = new LinkedHashMap<>();

    public YahooFinanceRequest() { }

    public YahooFinanceRequest(String ticker) {
        this.ticker = ticker;
    }

    public YahooFinanceRequest(String ticker, YahooEndpoint endpoint) {
        this.ticker = ticker;
        this.endpoint = endpoint;
    }

    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }

    public YahooEndpoint getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(YahooEndpoint endpoint) {
        this.endpoint = endpoint;
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


    public static class Builder {

        private YahooEndpoint endpoint = null;

        // use collection to allow for case where some endpoints allow multiple ticker values
        private Set<String> tickers = new LinkedHashSet<>();  // preserve insertion order
        private Map<String,String> paramMap = new LinkedHashMap<>();

        private Set<YahooModule> modules = new LinkedHashSet<>(); // only applicable for QuoteSummary


        public Builder() { }

        public Builder withTicker(String... tickers) {
            if (tickers != null) {
                this.tickers.addAll(Arrays.asList(tickers));
            }
            return this;
        }

        public Builder withEndpoint(YahooEndpoint endpoint) {
            this.endpoint = endpoint;
            return this;
        }

        public Builder withModules(YahooModule... modules) {
            if (modules != null && modules.length > 0) {

                if (this.endpoint == null) {
                    this.endpoint = YahooEndpoint.QUOTE_SUMMARY;  // for convenience (or laziness)
                }
                this.modules.addAll(Arrays.asList(modules));
            }
            else {
                this.modules.clear();
            }
            return this;
        }

        public Builder addParameter(String key, String value) {
            this.paramMap.put(key, value);
            return this;
        }

        public YahooFinanceRequest build() {
            YahooFinanceRequest req = new YahooFinanceRequest();
            req.setEndpoint(this.endpoint);

            if (this.tickers.size() > 0 && this.endpoint != null) {
                if (endpoint.isSupportsMultipleTickers()) {
                    req.setTicker(String.join(",", this.tickers));
                }
                else {
                    req.setTicker(this.tickers.iterator().next());
                }
            }
            else {
                req.setTicker("");
            }

            if (YahooEndpoint.QUOTE_SUMMARY.equals(this.endpoint) && this.modules.size() > 0)
            {
               req.addParam("modules", generateModuleList(this.modules));
            }

            req.addParams(paramMap);
            return req;
        }

        private String generateModuleList(Set<YahooModule> modules)
        {
            StringBuilder sb = new StringBuilder();
            for (YahooModule module : modules) {
                if (sb.length() > 0) {
                    sb.append(',');
                }
                sb.append(module.getName());
            }
            return sb.toString();
        }
    }

}
