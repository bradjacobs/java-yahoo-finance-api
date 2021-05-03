/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;

import bwj.yahoofinance.enums.YahooEndpoint;
import bwj.yahoofinance.enums.Type;

import java.util.LinkedHashMap;
import java.util.Map;

public class YahooLookupRequest extends YahooFinanceRequest {
    private static final String KEY_QUERY = "query";
    private static final String KEY_FORMATTED = "formatted";
    private static final String KEY_TYPE = "type";
    private static final String KEY_START = "start";
    private static final String KEY_COUNT = "count";

    private final int count;
    private final int start;

    // can add these later
//    private static final String KEY_LANG = "lang";
//    private static final String KEY_REGION = "region";
//    private static final String KEY_CORS_DOMAIN = "corsDomain";

    protected YahooLookupRequest(Builder builder)
    {
        this(builder.getEndpoint(), builder.generateParamMap(), builder.getCount(), builder.getStart());
    }

    protected YahooLookupRequest(YahooEndpoint endpoint, Map<String,String> paramMap, int count, int start)
    {
        super(endpoint, "", paramMap);
        this.count = count;
        this.start = start;
        paramMap.put(KEY_COUNT, String.valueOf(count));
        paramMap.put(KEY_START, String.valueOf(start));
    }



    public YahooLookupRequest createNextBatchRequest()
    {
        Map<String,String> paramMapCopy = new LinkedHashMap<>(this.paramMap);
        return new YahooLookupRequest(this.getEndpoint(), paramMapCopy, count, (start+count));
    }

    public int getCount() {
        return count;
    }

    public int getStart() {
        return start;
    }


    public static class Builder
    {
        private String query;
        private Boolean formatted;
        private Type type;
        private int count = 20; // default
        private int start = 0; // default
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
        public Builder withCount(int count) {
            this.count = Math.max(count, 0); // no negative allowed
            return this;
        }
        public Builder withStart(int start) {
            this.start = Math.max(start, 0); // no negative allowed
            return this;
        }

        public Builder addParameter(String key, String value) {
            this.paramMap.put(key, value);
            return this;
        }

        private YahooEndpoint getEndpoint() {
            if (includeTotalsOnly) {
                return YahooEndpoint.LOOKUP_TOTALS;
            }
            else {
                return YahooEndpoint.LOOKUP;
            }
        }

        private Map<String,String> generateParamMap()
        {
            Map<String,String> map = new LinkedHashMap<>();
            if (this.query != null) {
                map.put(KEY_QUERY, query.trim());
            }
            if (! this.includeTotalsOnly)
            {
                if (this.type != null) {
                    map.put(KEY_TYPE, type.toString().toLowerCase());
                }
                if (this.formatted != null) {
                    map.put(KEY_FORMATTED, formatted.toString().trim().toLowerCase());
                }
                map.putAll(paramMap);
            }
            return map;
        }

        public int getCount() {
            return count;
        }

        public int getStart() {
            return start;
        }


        public YahooLookupRequest build() {
            YahooLookupRequest req = new YahooLookupRequest(this);

            return req;
        }

    }
}