/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;


import bwj.yahoofinance.types.YahooEndpoint;
import bwj.yahoofinance.types.YahooModule;
import bwj.yahoofinance.request.builder.BaseRequestParamBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class YahooFinanceRequest
{
    protected final YahooEndpoint endpoint;
    protected final String ticker;
    protected final Map<String,String> paramMap;

    protected YahooFinanceRequest(YahooEndpoint endpoint, String ticker, Map<String,String> paramMap)
    {
        this.endpoint = endpoint;
        this.ticker = ticker;
        this.paramMap = paramMap;
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


    public static class Builder extends BaseRequestParamBuilder<Builder>
    {
        private YahooEndpoint endpoint = null;

        // use collection to allow for case where some endpoints allow multiple ticker values
        private Set<String> tickers = new LinkedHashSet<>();  // preserve insertion order
        private Set<YahooModule> modules = new LinkedHashSet<>(); // only applicable for QuoteSummary


        public Builder() { }

        public Builder withTicker(String... tickers) {
            if (tickers != null) {
                List<String> tickerList = Arrays.asList(tickers);
                tickerList.replaceAll(String::toUpperCase);  // make them all UPPERCASE
                this.tickers.addAll(tickerList);
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

        public YahooFinanceRequest build() {

            String tickerStr = generateTickerString();
            Map<String, String> paramMap = buildParamMap();

            YahooFinanceRequest req = new YahooFinanceRequest(this.endpoint, tickerStr, paramMap);
            validateRequestObject(req);
            return req;
        }


        private void validateRequestObject(YahooFinanceRequest req) {
            // TBD
        }

        private String generateTickerString() {
            if (this.tickers.size() > 0 && this.endpoint != null) {
                if (endpoint.getSupportsMultipleTickers()) {
                    return String.join(",", this.tickers);
                }
                else {
                    return this.tickers.iterator().next();
                }
            }
            return "";
        }



        @Override
        protected Builder getThis() {
            return this;
        }

        @Override
        protected Map<String, String> buildRequestSpecificMap() {

            Map<String,String> requestParamMap = new LinkedHashMap<>();
            if (YahooEndpoint.QUOTE_SUMMARY.equals(this.endpoint) && this.modules.size() > 0)
            {
                requestParamMap.put(ParamKeys.MODULES, generateModuleList(this.modules));
            }
            return requestParamMap;
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
