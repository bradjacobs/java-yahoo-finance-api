package bwj.yahoofinance.request.builder;

import bwj.yahoofinance.types.Type;
import bwj.yahoofinance.types.YahooEndpoint;

import java.util.LinkedHashMap;
import java.util.Map;

public class LookupBuilder extends BaseRequestBuilder<LookupBuilder>
{
    private static final int DEFAULT_COUNT = 20;
    private static final int DEFAULT_START = 0;


    private String query;
    private Boolean formatted;
    private Type type;
    private int count = DEFAULT_COUNT;
    private int start = DEFAULT_START;
    private boolean includeTotalsOnly = false;


    private boolean hasBeenBuilt = false; //internal flag


    public LookupBuilder withQuery(String query) {
        this.query = query;
        return this;
    }
    public LookupBuilder withTotalsOnly(boolean includeTotalsOnly) {
        this.includeTotalsOnly = includeTotalsOnly;
        return this;
    }
    public LookupBuilder withFormatted(Boolean formatted) {
        this.formatted = formatted;
        return this;
    }
    public LookupBuilder withType(Type type) {
        this.type = type;
        return this;
    }
    public LookupBuilder withCount(int count) {
        this.count = Math.max(count, 0); // no negative allowed
        return this;
    }
    public LookupBuilder withStart(int start) {
        this.start = Math.max(start, 0); // no negative allowed
        return this;
    }

    public int getCount() {
        return count;
    }

    public int getStart() {
        return start;
    }


    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        if (includeTotalsOnly) {
            return YahooEndpoint.LOOKUP_TOTALS;
        }
        return YahooEndpoint.LOOKUP;
    }

    @Override
    protected String _getRequestTicker()
    {
        return "";
    }

    @Override
    protected Map<String, String> _buildParamMap()
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


    @Override
    public YahooFinanceRequest build() {
        YahooFinanceRequest req = super.build();
        hasBeenBuilt = true;
        return req;
    }


    public YahooFinanceRequest buildNext()
    {
        if (hasBeenBuilt) {
            start += count;
        }
        return build();
    }

    @Override
    protected LookupBuilder getThis() {
        return this;
    }
}
