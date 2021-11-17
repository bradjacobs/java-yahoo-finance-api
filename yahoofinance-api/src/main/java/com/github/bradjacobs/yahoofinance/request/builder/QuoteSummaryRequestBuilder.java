package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.YahooModule;

import java.util.*;

public class QuoteSummaryRequestBuilder extends BaseRequestBuilder<QuoteSummaryRequestBuilder>
{
    private String ticker;
    private final Set<YahooModule> modules = new LinkedHashSet<>(); // only applicable for QuoteSummary

    @Override
    protected List<String> getRequiredParameters() {
        return Collections.singletonList(ParamKeys.MODULES);
    }

    public QuoteSummaryRequestBuilder withTicker(String ticker) {
        this.ticker = ticker;
        return this;
    }

    public QuoteSummaryRequestBuilder withModules(YahooModule... modules) {
        if (modules != null && modules.length > 0) {

            this.modules.addAll(Arrays.asList(modules));
        }
        else {
            this.modules.clear();
        }
        return this;
    }


    @Override
    public YahooEndpoint getEndpoint()
    {
        return YahooEndpoint.QUOTE_SUMMARY;
    }

    @Override
    protected String getRequestTicker()
    {
        return this.ticker;
    }


    @Override
    protected Map<String, String> buildEndpointParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();
        String moduleListString = generateModuleList(this.modules);
        if (moduleListString.length() > 0) {
            requestParamMap.put(ParamKeys.MODULES, moduleListString);
        }
        return requestParamMap;
    }

    @Override
    protected QuoteSummaryRequestBuilder getThis()
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
