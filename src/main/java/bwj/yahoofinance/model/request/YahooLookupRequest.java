/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.model.request;

import bwj.yahoofinance.YahooEndpoint;
import bwj.yahoofinance.model.params.Type;

import java.util.LinkedHashMap;
import java.util.Map;

public class YahooLookupRequest extends YahooFinanceRequest {
    private static final String KEY_QUERY = "query";
    private static final String KEY_FORMATTED = "formatted";
    private static final String KEY_TYPE = "type";
    private static final String KEY_START = "start";
    private static final String KEY_COUNT = "count";

    // can add these later
//    private static final String KEY_LANG = "lang";
//    private static final String KEY_REGION = "region";
//    private static final String KEY_CORS_DOMAIN = "corsDomain";

    public YahooLookupRequest() {
        super("", YahooEndpoint.LOOKUP);
    }

    public YahooLookupRequest(boolean totalsOnly) {
        super("", getEndpoint(totalsOnly));
    }

    private static YahooEndpoint getEndpoint(boolean includeTotalsOnly) {
        if (includeTotalsOnly) {
            return YahooEndpoint.LOOKUP_TOTALS;
        }
        else {
            return YahooEndpoint.LOOKUP;
        }
    }


    public Integer getStart() {
        String startValue = paramMap.get(KEY_START);
        return (startValue != null ? Integer.valueOf(startValue) : null);
    }

    public void setStart(Integer start) {
        if (start != null) {
            paramMap.put(KEY_START, start.toString());
        }
    }

    public Integer getCount() {
        String countValue = paramMap.get(KEY_COUNT);
        return (countValue != null ? Integer.valueOf(countValue) : null);
    }

    public void setCount(Integer count) {
        if (count != null) {
            paramMap.put(KEY_COUNT, count.toString());
        }
    }



    @Override
    public void setTicker(String ticker) {
        // ignore
    }

    @Override
    public void addEndpoint(YahooEndpoint... endpoints) {
        // ignore
    }

    public static class Builder
    {
        private String query;
        private Boolean formatted;
        private Type type;
        private Integer start;
        private Integer count;
        private boolean includeTotalsOnly = false;

        // for any other misc params
        private Map<String,String> paramMap = new LinkedHashMap<>();

        public Builder withQuery(String query) {
            this.query = query;
            return this;
        }
        public Builder withTotalsOnly(boolean includeTotalsOnly) {
            this.includeTotalsOnly = includeTotalsOnly;
            return this;
        }
        public Builder withFormatted(Boolean formatted) {
            this.formatted = formatted;
            return this;
        }
        public Builder withType(Type type) {
            this.type = type;
            return this;
        }
        public Builder withStart(Integer start) {
            this.start = start;
            return this;
        }
        public Builder withCount(Integer count) {
            this.count = count;
            return this;
        }

        public Builder addParameter(String key, String value) {
            this.paramMap.put(key, value);
            return this;
        }

        public YahooLookupRequest build() {
            YahooLookupRequest req = new YahooLookupRequest(this.includeTotalsOnly);

            if (this.query != null) {
                req.addParam(KEY_QUERY, query.trim());
            }
            if (! this.includeTotalsOnly)
            {
                if (this.formatted != null) {
                    req.addParam(KEY_FORMATTED, formatted.toString().trim().toLowerCase());
                }
                if (this.type != null) {
                    req.addParam(KEY_TYPE, type.toString().toLowerCase());
                }
                req.setStart(this.start);
                req.setCount(this.count);

                req.addParams(paramMap);
            }
            return req;
        }

    }
}