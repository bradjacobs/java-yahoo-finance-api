package com.github.bradjacobs.yahoofinance;

import com.github.bradjacobs.yahoofinance.http.HttpClientAdapter;
import com.github.bradjacobs.yahoofinance.http.HttpCommonsClientAdapter;
import com.github.bradjacobs.yahoofinance.http.Response;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;

import javax.security.auth.login.LoginException;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;


/**
 * Helper method to "Login" to yahoo.  Namely to allow use of extra functionality if you have a 'premium' membership.
 *   The 'login' will essentially add a cookie to the client to represent the client is logged in.
 *   It is assumed that the nested httpClient is configured to handle '302/Redirects'.
 *
 *  ***********************
 *  *****   UPDATE   ******
 *  ***********************
 *  This will not always work!  Namely yahoo may prompt for a 'capcha' to prove not a robot.
 *    Capchas were not encountered during the initial implementation of this code, however it's not surprising that yahoo wants
 *    to have limits on robot automation.
 *
 *  Alternatives will be researched when time allows.
 *
 */
public class YahooLoginExecutor
{
    private static final String LOGIN_BASE_URL = "https://login.yahoo.com";

    private static final String PAYLOAD_TEMPLATE_1 = "crumb=%s&acrumb=%s&sessionIndex=%s&username=%s&passwd=&signin=Next&persistent=y";

    // todo: string below with "nornaml" a type or did that on purpose?
    private static final String PAYLOAD_TEMPLATE_2 = "crumb=%s&acrumb=%s&sessionIndex=%s&username=%s&passwordContext=nornaml&password=%s&verifyPassword=Next";

    private static final Map<String,String> DEFAULT_HEADER_MAP = new TreeMap<String,String>() {{
        put("Accept", "*/*");
        put("Accept-Encoding", "gzip, deflate");
        put("Accept-Language", "en-US,en;q=0.9");
        put("Cache-Control", "no-cache");
        put("Connection", "keep-alive");
        put("Content-type", "application/x-www-form-urlencoded; charset=UTF-8");  //
        put("Pragma", "no-cache");

        // note: would like to change the useragent value, but login will fail if you just "make one up"
        put("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.164 Safari/537.36");
    }};

    private static final long SLEEP_BETWEEN_REQUESTS = 300L;




    private final HttpClientAdapter httpClient;
    private final String userName;
    private final String password;

    private boolean isLoggedIn = false;


    public YahooLoginExecutor(HttpClientAdapter httpClient, String userName, String password)
    {
        if (httpClient == null) {
            throw new IllegalArgumentException("Must provide an httpClientAdapter.");
        }
        if (StringUtils.isEmpty(userName)) {
            throw new IllegalArgumentException("Must provide a username.");
        }
        if (StringUtils.isEmpty(password)) {
            throw new IllegalArgumentException("Must provide a password.");
        }
        if (! (httpClient instanceof HttpCommonsClientAdapter)) {
            throw new NotImplementedException("Only Apache Commons is supported for login (at present)");
        }


        this.httpClient = httpClient;
        this.userName = userName;
        this.password = password;
    }

    public void doLogin() throws Exception
    {
        // REQUEST #1...
        //    loads in required Cookies + need to grab some crumb/session values from the response.
        //
        Response responseA = executeRequest(LOGIN_BASE_URL, DEFAULT_HEADER_MAP);
        String responseStringA = responseA.getBody();

        HiddenSessionValues hiddenSessionValues = extractHiddenSessionInfo(responseStringA);
        Thread.sleep(SLEEP_BETWEEN_REQUESTS);



        //  REQUEST #2
        //     Now call login as POST w/ a special payload & username.
        //     The result gets the next url need to hit.
        //

        // need to add an 'extra' header for this specific request
        Map<String,String> requestHeaderMap = new TreeMap<>(DEFAULT_HEADER_MAP);
        requestHeaderMap.put("X-Requested-With", "XMLHttpRequest");

        String payload = String.format(PAYLOAD_TEMPLATE_1,
            hiddenSessionValues.getCrumb(), hiddenSessionValues.getAcrumb(), hiddenSessionValues.getSessionIndex(), userName);

        Response responseB = executeRequest(LOGIN_BASE_URL, payload, requestHeaderMap);
        String responseStringB = responseB.getBody();

        // grab the url location from the response.  (side note:  seems little weird this isn't a 3xx response)
        String parsedUrl = StringUtils.substringBetween(responseStringB, "{\"location\":\"", "\"}");

        Thread.sleep(SLEEP_BETWEEN_REQUESTS);



        //  REQUEST #3
        //     Final call with required payload and username + password.
        //
        //    ** NOTE **
        //    this will produce '302' forward responses.
        //    =====> At present it is 'assumed' that the underlying httpClient is configured to handle redirects  < ======

        String finalLoginUrl = LOGIN_BASE_URL + parsedUrl;
        String finalLoginPayload = String.format(PAYLOAD_TEMPLATE_2,
            hiddenSessionValues.getCrumb(), hiddenSessionValues.getAcrumb(), hiddenSessionValues.getSessionIndex(),
            userName, password);

        Response responseC = executeRequest(finalLoginUrl, finalLoginPayload, DEFAULT_HEADER_MAP);
        String responseStringC = responseC.getBody();


        // if the login succeeded then the response page will contain the username and a message to manage accounts.
        if (!responseStringC.contains("Manage your Yahoo accounts")   ||  !responseStringC.contains(userName)) {
            throw new LoginException("Unable to Login!!");
        }


        // if made it here, then login succeeded!
        this.isLoggedIn = true;
    }

    public boolean isLoggedIn()
    {
        return isLoggedIn;
    }

    private HiddenSessionValues extractHiddenSessionInfo(String responseBody)
    {
        String crumb = StringUtils.substringBetween(responseBody, "type=\"hidden\" name=\"crumb\" value=\"", "\"");
        String acrumb = StringUtils.substringBetween(responseBody, "type=\"hidden\" name=\"acrumb\" value=\"", "\"");
        String sessionIndex = StringUtils.substringBetween(responseBody, "type=\"hidden\" name=\"sessionIndex\" value=\"", "\"");

        return new HiddenSessionValues(crumb, acrumb, sessionIndex);
    }



    private Response executeRequest(String url, Map<String,String> headerMap) throws IOException
    {
        return executeRequest(url, null, headerMap);
    }

    private Response executeRequest(String url, String postBody, Map<String,String> headerMap) throws IOException
    {
        Response response = null;
        if (StringUtils.isNotEmpty(postBody)) {
            response = this.httpClient.executePost(url, postBody, headerMap);
        }
        else {
            response = this.httpClient.executeGet(url, headerMap);
        }

        if (isErrorResponse(response)) {
            throw new RuntimeException("Unable to login to yahoo!");
        }

        return response;

    }


    private boolean isErrorResponse(Response response)
    {
        int code = response.getCode();
        if (code >= 400) {
            return true;
        }

        String responseBody = response.getBody();
        if (responseBody.contains("Uh-oh...")) {
            return true;
        }
        // todo - examine weird whitespace in string
        if (responseBody.contains("Please try again")) {
            return true;
        }

        return false;
    }


    private static class HiddenSessionValues {
        private final String crumb;
        private final String acrumb;
        private final String sessionIndex;

        public HiddenSessionValues(String crumb, String acrumb, String sessionIndex) {
            this.crumb = crumb;
            this.acrumb = acrumb;
            this.sessionIndex = sessionIndex;
        }

        public String getCrumb() {
            return crumb;
        }

        public String getAcrumb() {
            return acrumb;
        }

        public String getSessionIndex() {
            return sessionIndex;
        }
    }

}
