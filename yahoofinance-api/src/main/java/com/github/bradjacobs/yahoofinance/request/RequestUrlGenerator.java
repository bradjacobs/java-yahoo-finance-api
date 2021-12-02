package com.github.bradjacobs.yahoofinance.request;

import com.github.bradjacobs.yahoofinance.request.builder.ParamKeys;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class RequestUrlGenerator
{
    private static final String BASE_API_SCHEME = "https";
    private static final String BASE_API_HOST = "query1.finance.yahoo.com";

    private final CrumbDataSource crumbDataSource;

    public RequestUrlGenerator() {
        this(null);
    }
    public RequestUrlGenerator(CrumbDataSource crumbDataSource) {
        this.crumbDataSource = crumbDataSource;
    }


    public String buildRequestUrl(YahooRequest request) throws IOException {

        Map<String, String> paramMap = new LinkedHashMap<>(request.getParamMap());
        if (request.isCrumbRequired() && crumbDataSource != null) {
            String crumb = crumbDataSource.getCrumb(request);
            paramMap.put(ParamKeys.CRUMB, crumb);
        }

        YahooEndpoint endpoint = request.getEndpoint();

        //  e.g. /v8/finance/chart/AAPL
        String urlPath = endpoint.getPathPrefix() + "v" + endpoint.getVersion() + "/finance/" + endpoint.getName();
        if ( endpoint.isTickerOnPath() ) {
            urlPath +=  "/" + request.getTicker().toUpperCase();
        }

        URIBuilder builder = new URIBuilder()
                .setScheme(BASE_API_SCHEME)
                .setHost(BASE_API_HOST)
                .setPath(urlPath);

        for (Map.Entry<String, String> paramEntry : paramMap.entrySet()) {
            builder.addParameter(paramEntry.getKey(), paramEntry.getValue());
        }
        return builder.toString();
    }
}
