package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.http.Response;
import com.github.bradjacobs.yahoofinance.response.converter.YahooResponseConverter;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.util.PrettyFormatter;

import java.util.Collections;
import java.util.List;

public class YahooResponse extends AbstractResponse
{
    private Response rawResponse;

    public YahooResponse(YahooEndpoint yahooEndpoint, Response rawResponse)
    {
        super(yahooEndpoint);
        this.rawResponse = rawResponse;
    }

    public YahooResponse(YahooEndpoint endpoint, ResponseConverterConfig converterConfig, Response rawResponse)
    {
        super(endpoint, converterConfig);
        this.rawResponse = rawResponse;
    }

    @Override
    protected List<Response> getListResponse()
    {
        return Collections.singletonList(this.rawResponse);
    }

    public int getHttpCode() {
        return this.rawResponse.getCode();
    }

    public String getJson() {
        return this.rawResponse.getBody();
    }

    public String getPrettyJson() {
        return PrettyFormatter.prettyJson(this.rawResponse.getBody());
    }

    @Override
    public boolean hasErrors()
    {
        return rawResponse.isError();
    }
}
