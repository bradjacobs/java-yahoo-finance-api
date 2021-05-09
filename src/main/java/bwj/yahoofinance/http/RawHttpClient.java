package bwj.yahoofinance.http;

import java.util.Map;

public interface RawHttpClient
{
    String executeGet(String url, Map<String,String> requestheaders);

    String executePost(String url, String postBody, Map<String,String> requestheaders);
}
