/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import bwj.yahoofinance.http.RawClientFactory;
import bwj.yahoofinance.http.RawHttpClient;
import bwj.yahoofinance.types.YahooEndpoint;
import bwj.yahoofinance.request.builder.YahooFinanceRequest;
import bwj.yahoofinance.validation.YahooRequestValidator;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class YahooFinanceClient
{
    private static final String BASE_API_SCHEME = "https";
    private static final String BASE_API_HOST = "query1.finance.yahoo.com";

    private static final String DEFAULT_CONTENT_TYPE = "application/json";
    private static final String DEFAULT_USER_AGENT = "Java-Http-Client/11.0.0";

    private Map<String,String> requestHeaderMap = new LinkedHashMap<>();

    private static final YahooRequestValidator requestValidator = new YahooRequestValidator();

    // rawHttpClient is a simple interface around the 'true' httpClient.
    private RawHttpClient rawHttpClient;


    public YahooFinanceClient()
    {
        setContentType(DEFAULT_CONTENT_TYPE);
        setUserAgent(DEFAULT_USER_AGENT);
        setInternalClient(RawClientFactory.createDefaultClient());
    }

    public void setContentType(String contentType) {
        requestHeaderMap.put("Content-Type", contentType);
    }

    public void setUserAgent(String userAgent) {
        requestHeaderMap.put("User-Agent", userAgent);
    }

    public String executeRequest(YahooFinanceRequest request)
    {
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);
        return rawHttpClient.executeGet(url, this.requestHeaderMap);
    }


    protected String buildRequestUrl(YahooFinanceRequest request)
    {
        Map<String, String> paramMap = request.getParamMap();
        if (paramMap == null) {
            paramMap = Collections.emptyMap();
        }

        String ticker = request.getTicker().toUpperCase();
        YahooEndpoint endpoint = request.getEndpoint();


        // todo: handle request w/ POST + crumb

        URIBuilder builder = new URIBuilder();
        builder.setScheme(BASE_API_SCHEME);
        builder.setHost(BASE_API_HOST);

        //  e.g. /v8/finance/chart/AAPL
        String path = endpoint.getPathPrefix() + "v" + endpoint.getVersion() + "/finance/" + endpoint.getName();

        // check if need to append the ticker to the path itself.
        if ( endpoint.isTickerOnPath() )
        {
            path +=  "/" + ticker;
        }

        builder.setPath(path);

        for (Map.Entry<String, String> paramEntry : paramMap.entrySet()) {
            builder.addParameter(paramEntry.getKey(), paramEntry.getValue());
        }

        String url = builder.toString();
        return url;
    }


    public void setInternalClient(CloseableHttpClient client) {
        setInternalClient(RawClientFactory.createRawHttpClient(client));
    }
    public void setInternalClient(RawHttpClient client) {
        if (client == null) {
            throw new IllegalArgumentException("Cannot set the internal client to null.");
        }
        this.rawHttpClient = client;
    }

}
