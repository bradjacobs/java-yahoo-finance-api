/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance;

import bwj.yahoofinance.http.HttpClientAdapterFactory;
import bwj.yahoofinance.http.HttpClientAdapter;
import bwj.yahoofinance.http.Response;
import bwj.yahoofinance.types.YahooEndpoint;
import bwj.yahoofinance.request.builder.YahooFinanceRequest;
import bwj.yahoofinance.validation.YahooRequestValidator;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;

import java.io.IOException;
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
    private HttpClientAdapter rawHttpClient;


    public YahooFinanceClient()
    {
        setContentType(DEFAULT_CONTENT_TYPE);
        setUserAgent(DEFAULT_USER_AGENT);
        setInternalClient(HttpClientAdapterFactory.createDefaultClient());
    }

    public void setContentType(String contentType) {
        requestHeaderMap.put("Content-Type", contentType);
    }

    public void setUserAgent(String userAgent) {
        requestHeaderMap.put("User-Agent", userAgent);
    }

    public String executeRequest(YahooFinanceRequest request) throws IOException
    {
        // validation will throw an exception if invalid request is detected
        requestValidator.validationRequest(request);

        String url = buildRequestUrl(request);

        Response response = rawHttpClient.executeGet(url, this.requestHeaderMap);
        if (response.isError()) {
            // TODO - come back and handle better
            throw new RuntimeException("Error occurred during request: ");
        }
        return response.getBody();
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
        setInternalClient(HttpClientAdapterFactory.createHttpClient(client));
    }
    public void setInternalClient(HttpClientAdapter client) {
        if (client == null) {
            throw new IllegalArgumentException("Cannot set the internal client to null.");
        }
        this.rawHttpClient = client;
    }

}
