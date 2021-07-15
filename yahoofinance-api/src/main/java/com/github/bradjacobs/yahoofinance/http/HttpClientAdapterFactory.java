package com.github.bradjacobs.yahoofinance.http;

import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import org.apache.http.client.config.CookieSpecs;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.params.CookiePolicy;
import org.apache.http.client.params.HttpClientParams;
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
        return new HttpCommonsClientAdapter( createInternalHttpCommonsClient() );
    }
    public static HttpClientAdapter createDefaultHttpCommonsClient() {
        return new HttpCommonsClientAdapter( createInternalHttpCommonsClient() );
    }
    public static HttpClientAdapter createDefaultOkHttpClient() {
        return new OkHttpClientAdapter( createInternalOkHttpClient() );
    }



    public static HttpClientAdapter createHttpClient(CloseableHttpClient apacheClientj)
    {
        return new HttpCommonsClientAdapter(apacheClientj);
    }

    public static HttpClientAdapter createHttpClient(OkHttpClient okHttpClient)
    {
        return new OkHttpClientAdapter(okHttpClient);
    }



    // todo - cleanup / remove below

    // todo: values are arbitrary
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

        CloseableHttpClient httpClient = HttpClientBuilder.create()
            //.setKeepAliveStrategy() // default should be fine
            .setDefaultRequestConfig(config)
            .setMaxConnPerRoute(MAX_CONNECTIONS_PER_HOST)
            .setMaxConnTotal(MAX_TOTAL_CONNECTIONS)
            //.setRedirectStrategy(new LaxRedirectStrategy())   // todo research if this is needed
            .build();

        return httpClient;
    }


    private static OkHttpClient createInternalOkHttpClient()
    {
        HashMap<String, List<Cookie>> cookieStore = new HashMap<>();
        return new OkHttpClient.Builder()
            .connectTimeout(CONNECTION_TIMEOUT, TimeUnit.MILLISECONDS)
            .readTimeout(READ_TIMEOUT, TimeUnit.MILLISECONDS)
            .addInterceptor(new HttpLoggingInterceptor())  // todo: experiment w/ what this does/doesn't do
            .cookieJar(new SimpleCookieJar())
            .build();
    }

    // needed for "screener" requests
    private static class SimpleCookieJar implements CookieJar
    {
        private final Map<String, List<Cookie>> cookieStore;
        SimpleCookieJar() {
            cookieStore = new HashMap<>();
        }

        @Override
        public void saveFromResponse(HttpUrl url, List<Cookie> cookies) {
            cookieStore.put(url.host(), cookies);
        }

        @Override
        public List<Cookie> loadForRequest(HttpUrl url)
        {
            String host = url.host();
            //   if asking for cookie for "query1.finance.yahoo.com", then give cookie for "finance.yahoo.com"
            // todo: make cleaner when time allows
            host = host.replace("query1.", "");
            host = host.replace("query2.", "");

            List<Cookie> cookies = cookieStore.get(host);
            return cookies != null ? cookies : new ArrayList<Cookie>();
        }
    }

}
