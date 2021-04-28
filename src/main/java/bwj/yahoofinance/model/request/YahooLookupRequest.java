package bwj.yahoofinance.model.request;

import bwj.yahoofinance.YahooEndpoint;

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

    public String getQuery() {
        return paramMap.get(KEY_QUERY);
    }

    public void setQuery(String query) {
        paramMap.put(KEY_QUERY, query);
    }

    public Boolean getFormatted() {
        String formattedValue = paramMap.get(KEY_FORMATTED);
        return (formattedValue != null ? Boolean.valueOf(formattedValue) : null);
    }

    public void setFormatted(Boolean formatted) {
        if (formatted != null) {
            paramMap.put(KEY_FORMATTED, formatted.toString());
        }
    }

    public String getType() {
        return paramMap.get(KEY_TYPE);
    }

    public void setType(String type) {
        paramMap.put(KEY_TYPE, type);
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
        private String type;
        private Integer start;
        private Integer count;

        // for any other misc params
        private Map<String,String> paramMap = new LinkedHashMap<>();

        public Builder withQuery(String query) {
            this.query = query;
            return this;
        }
        public Builder withFormatted(Boolean formatted) {
            this.formatted = formatted;
            return this;
        }
        public Builder withType(String type) {
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
            YahooLookupRequest req = new YahooLookupRequest();
            req.setQuery(this.query);
            req.setFormatted(this.formatted);
            req.setType(this.type);
            req.setStart(this.start);
            req.setCount(this.count);

            req.addParams(paramMap);
            return req;
        }


    }
}