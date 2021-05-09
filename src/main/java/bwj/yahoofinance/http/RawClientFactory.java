package bwj.yahoofinance.http;

import org.apache.http.client.config.RequestConfig;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;

public class RawClientFactory
{

    public static RawHttpClient createDefaultClient() {
        return new RawApacheClient( createDefaultHttpClient() );
    }

    public static RawHttpClient createRawHttpClient(CloseableHttpClient apacheClientj)
    {
        return new RawApacheClient(apacheClientj);
    }



    // todo: values are arbitrary
    private static final int MAX_CONNECTIONS_PER_HOST = 10;
    private static final int MAX_TOTAL_CONNECTIONS = 10;
    private static final int CONNECTION_TIMEOUT = 20000;
    private static final int READ_TIMEOUT = 30000;

    private static CloseableHttpClient createDefaultHttpClient()
    {
        RequestConfig config = RequestConfig.custom()
            .setConnectTimeout(CONNECTION_TIMEOUT)
            .setConnectionRequestTimeout(CONNECTION_TIMEOUT)
            .setSocketTimeout(READ_TIMEOUT).build();

        CloseableHttpClient httpClient = HttpClientBuilder.create()
            //.setKeepAliveStrategy() // default should be fine
            .setDefaultRequestConfig(config)
            .setMaxConnPerRoute(MAX_CONNECTIONS_PER_HOST)
            .setMaxConnTotal(MAX_TOTAL_CONNECTIONS)
            .build();

        return httpClient;
    }


}
