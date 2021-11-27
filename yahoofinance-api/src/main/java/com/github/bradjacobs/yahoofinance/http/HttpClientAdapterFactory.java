package com.github.bradjacobs.yahoofinance.http;

import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import org.apache.http.client.config.CookieSpecs;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.LaxRedirectStrategy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class HttpClientAdapterFactory
{
    public static HttpClientAdapter createDefaultClient() {
        return createDefaultHttpCommonsClient();
    }
    public static HttpClientAdapter createDefaultHttpCommonsClient() {
        return createHttpClient( createInternalHttpCommonsClient() );
    }
    public static HttpClientAdapter createDefaultOkHttpClient() {
        return createHttpClient( createInternalOkHttpClient() );
    }

    /**
     * Creates httpClientAdapter for Apache Commons Http Client
     * @param apacheClient httpClient
     * @return httpClientAdapter
     */
    public static HttpClientAdapter createHttpClient(CloseableHttpClient apacheClient) {
        return new HttpCommonsClientAdapter(apacheClient);
    }

    /**
     * Creates httpClientAdapter for OKHttpClient
     * @param okHttpClient httpClient
     * @return httpClientAdapter
     */
    public static HttpClientAdapter createHttpClient(OkHttpClient okHttpClient) {
        return new OkHttpClientAdapter(okHttpClient);
    }


    // NOTE: values are arbitrary!
    private static final int MAX_CONNECTIONS_PER_HOST = 10;
    private static final int MAX_TOTAL_CONNECTIONS = 10;
    private static final int CONNECTION_TIMEOUT = 20000;
    private static final int READ_TIMEOUT = 30000;

    private static CloseableHttpClient createInternalHttpCommonsClient()
    {
        RequestConfig config = RequestConfig.custom()
            .setConnectTimeout(CONNECTION_TIMEOUT)
            .setConnectionRequestTimeout(CONNECTION_TIMEOUT)
            .setSocketTimeout(READ_TIMEOUT)
            .setCookieSpec(CookieSpecs.STANDARD)
            .build();

        return HttpClientBuilder.create()
            //.setKeepAliveStrategy() // default should be fine
            .setDefaultRequestConfig(config)
            .setMaxConnPerRoute(MAX_CONNECTIONS_PER_HOST)
            .setMaxConnTotal(MAX_TOTAL_CONNECTIONS)
            .setRedirectStrategy(new LaxRedirectStrategy())  // allow redirect for all primary method types
            .build();
    }

    private static OkHttpClient createInternalOkHttpClient()
    {
        return new OkHttpClient.Builder()
            .connectTimeout(CONNECTION_TIMEOUT, TimeUnit.MILLISECONDS)
            .readTimeout(READ_TIMEOUT, TimeUnit.MILLISECONDS)
            .addInterceptor(new HttpLoggingInterceptor())  // todo: experiment w/ what this does/doesn't do
            .cookieJar(new SimpleCookieJar())
            .build();
    }

    // needed for "screener" requests
    private static class SimpleCookieJar implements CookieJar {
        private final Map<String, List<Cookie>> cookieStore = new HashMap<>();

        @Override
        public void saveFromResponse(HttpUrl url, List<Cookie> cookies) {
            cookieStore.put(url.host(), cookies);
        }

        @Override
        public List<Cookie> loadForRequest(HttpUrl url) {
            String host = url.host();
            //  if asking for cookie for "query1.finance.yahoo.com", then give cookie for "finance.yahoo.com"
            if (host.endsWith(".finance.yahoo.com")) {
                host = "finance.yahoo.com";
            }
            List<Cookie> cookies = cookieStore.get(host);
            return cookies != null ? cookies : new ArrayList<>();
        }
    }

}
