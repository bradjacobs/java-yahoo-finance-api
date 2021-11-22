package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.YahooModule;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class QuoteSummaryRequestBuilder extends BaseRequestBuilder<QuoteSummaryRequestBuilder>
{
    private String ticker;
    private final Set<YahooModule> modules = new LinkedHashSet<>();

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
    protected YahooEndpoint getEndpoint()
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
        return modules.stream().map(YahooModule::getName).collect(Collectors.joining(","));
    }
}
