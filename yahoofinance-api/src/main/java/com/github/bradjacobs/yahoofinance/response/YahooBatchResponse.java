package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.PrettyFormatter;

import java.util.ArrayList;
import java.util.List;

public class YahooBatchResponse extends AbstractResponse
{
    private List<Response> rawResponseList;

    public YahooBatchResponse(YahooEndpoint yahooEndpoint, List<Response> rawResponseList)
    {
        super(yahooEndpoint);
        this.rawResponseList = rawResponseList;
    }

    @Override
    protected List<Response> getListResponse()
    {
        return this.rawResponseList;
    }

    @Override
    public boolean hasErrors()
    {
        for (Response response : rawResponseList) {
            if (response.isError()) {
                return true;
            }
        }
        return false;
    }

    public List<String> getJson()
    {
       return makeJsonList(false);
    }

    public List<String> getPrettyJson()
    {
        return makeJsonList(true);
    }

    /**
     * Grab the response bodys from each object.
     * @param makePretty true if json should be pretty formatted.
     * @return list of Json responses in string format.
     */
    private List<String> makeJsonList(boolean makePretty)
    {
        List<String> jsonList = new ArrayList<>();
        for (Response response : rawResponseList) {
            String json = response.getBody();
            if (makePretty) {
                json = PrettyFormatter.prettyJson(json);
            }
            jsonList.add(json);
        }
        return jsonList;
    }

}
