package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.YahooModule;

import java.util.*;

public class QuoteSummaryBuilder extends BaseRequestBuilder<QuoteSummaryBuilder>
{
    private String ticker;
    private Set<YahooModule> modules = new LinkedHashSet<>(); // only applicable for QuoteSummary


    public QuoteSummaryBuilder withTicker(String ticker) {
        this.ticker = ticker;
        return this;
    }

    public QuoteSummaryBuilder withModules(YahooModule... modules) {
        if (modules != null && modules.length > 0) {

            this.modules.addAll(Arrays.asList(modules));
        }
        else {
            this.modules.clear();
        }
        return this;
    }


    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        return YahooEndpoint.QUOTE_SUMMARY;
    }

    @Override
    protected String _getRequestTicker()
    {
        return this.ticker;
    }


    @Override
    protected Map<String, String> _buildParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();
        String moduleListString = generateModuleList(this.modules);
        if (moduleListString.length() > 0) {
            requestParamMap.put(ParamKeys.MODULES, moduleListString);
        }
        return requestParamMap;
    }

    @Override
    protected QuoteSummaryBuilder getThis()
    {
        return this;
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
