/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;

import bwj.yahoofinance.request.builder.ParamKeys;
import bwj.yahoofinance.types.Type;
import bwj.yahoofinance.types.YahooEndpoint;
import bwj.yahoofinance.request.builder.BaseRequestParamMapBuilder;

import java.util.LinkedHashMap;
import java.util.Map;

public class YahooLookupRequest extends YahooFinanceRequest
{
    private static final int DEFAULT_COUNT = 20;
    private static final int DEFAULT_START = 0;

    protected YahooLookupRequest(YahooEndpoint endpoint, Map<String,String> paramMap)
    {
        super(endpoint, "", paramMap);
    }


    public static class Builder extends BaseRequestParamMapBuilder<Builder>
    {
        private String query;
        private Boolean formatted;
        private Type type;
        private int count = DEFAULT_COUNT;
        private int start = DEFAULT_START;
        private boolean includeTotalsOnly = false;


        private boolean hasBeenBuilt = false; //internal flag

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


        @Override
        protected Map<String, String> buildRequestSpecificMap()
        {
            Map<String,String> map = new LinkedHashMap<>();
            if (this.query != null) {
                map.put(ParamKeys.QUERY, query.trim());
            }
            if (! this.includeTotalsOnly)
            {
                if (this.type != null) {
                    map.put(ParamKeys.TYPE, type.toString().toLowerCase());
                }
                if (this.formatted != null) {
                    map.put(ParamKeys.FORMATTED, formatted.toString());
                }

                map.put(ParamKeys.COUNT, String.valueOf(count));
                map.put(ParamKeys.START, String.valueOf(start));
            }
            return map;
        }


        public int getCount() {
            return count;
        }

        public int getStart() {
            return start;
        }


        public YahooLookupRequest build()
        {
            YahooEndpoint endpoint = includeTotalsOnly ? YahooEndpoint.LOOKUP_TOTALS : YahooEndpoint.LOOKUP;
            YahooLookupRequest req = new YahooLookupRequest(endpoint, this.buildParamMap());
            hasBeenBuilt = true;
            return req;
        }

        public YahooLookupRequest buildNext()
        {
            if (hasBeenBuilt) {
                start += count;
            }
            return build();
        }

        @Override
        protected Builder getThis() {
            return this;
        }
    }
}