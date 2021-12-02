/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Class for common code for _ALL_ requests regarding building the parameter map.
 * @param <T>
 */
abstract public class BaseRequestBuilder<T extends BaseRequestBuilder<T>>
{
    private boolean includeRegionParam = true;
    private String region = Locale.getDefault().getCountry();

    // for any other misc params
    private final Map<String,String> extraParametersMap = new LinkedHashMap<>();

    private final Map<String,String> additionalHeaderMap = new LinkedHashMap<>();

    protected abstract T getThis();



    protected String getRegion() {
        return region;
    }

    public T includeRegionParam(boolean includeRegion) {
        this.includeRegionParam = includeRegion;
        return getThis();
    }

    public T withRegion(Locale locale) {
        if (locale != null) {
            return withRegion(locale.getCountry());
        }
        else {
            return withRegion("");
        }
    }

    public T withRegion(Region region) {
        if (region != null) {
            this.region = region.getCode();
        }
        return getThis();
    }

    protected T withRegion(String region) {
        if (StringUtils.isEmpty(region)) {
            throw new IllegalArgumentException("Region value cannot be blank/empty.");
        }
        this.region = region;
        return getThis();
    }

    public T addParam(String key, String value) {
        key = (key != null ? key.trim() : "");
        value = (value != null ? value.trim() : "");

        if (key.length() > 0) {
            if (value.length() > 0) {
                this.extraParametersMap.put(key, value.trim());
            }
            else {
                this.extraParametersMap.remove(key);
            }
        }

        return getThis();
    }

    public T addHeader(String key, String value) {
        if (key != null) {
            key = key.trim();
            if (value != null) {
                this.additionalHeaderMap.put(key, value.trim());
            }
            else {
                this.additionalHeaderMap.remove(key);
            }
        }
        return getThis();
    }

    public T addHeaders(Map<String,String> headerMap) {
        if (headerMap != null) {
            for (Map.Entry<String, String> entry : headerMap.entrySet()) {
                addHeader(entry.getKey(), entry.getValue());
            }
        }
        return getThis();
    }

    protected Map<String,String> buildParamMap()
    {
        Map<String, String> paramMap = buildEndpointParamMap();
        paramMap.putAll(this.extraParametersMap);

        if (includeRegionParam) {
            paramMap.put(ParamKeys.REGION, this.region);
        }
        return paramMap;
    }

    abstract protected Map<String,String> buildEndpointParamMap();
    abstract protected YahooEndpoint getEndpoint();
    abstract protected String getRequestTicker();

    public YahooRequest build() {

        YahooEndpoint endpoint = getEndpoint();
        String ticker = getRequestTicker();
        Map<String, String> paramMap = buildParamMap();
        Object postBody = buildRequestPostBody();

        YahooRequest req = generateRequest(endpoint, ticker, paramMap, postBody, additionalHeaderMap);
        validateRequest(req);
        return req;
    }

    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        return new YahooFinanceRequest(endpoint, ticker, paramMap, postBody, headerMap);
    }

    protected Object buildRequestPostBody() {
        return null;  // no post body by default
    }


    /**
     * Will throw exception if request is invalid
     * @param request request to be validated
     */
    // todo: might refactor out into a separate class/strategy
    protected void validateRequest(YahooRequest request)
    {
        YahooEndpoint endpoint = request.getEndpoint();
        if (endpoint == null) {
            throw new IllegalArgumentException("Request is missing endpoint value.");
        }
        if (endpoint.requiresTicker() && StringUtils.isEmpty(request.getTicker()) ) {
            throw new IllegalArgumentException("Request is missing a ticker value.");
        }

        List<String> requiredParameters = new ArrayList<>(getRequiredParameters());
        if (endpoint.isTickerKeyValueParam()) { requiredParameters.add(ParamKeys.SYMBOL); }
        if (endpoint.isMultiTickerSupported()) { requiredParameters.add(ParamKeys.SYMBOLS); }

        Map<String, String> paramMap = request.getParamMap() != null ? request.getParamMap() : Collections.emptyMap();
        for (String requiredParam : requiredParameters) {
            if (!paramMap.containsKey(requiredParam)) {
                throw new IllegalArgumentException(String.format("Endpoint %s is missing required parameter '%s'.", endpoint, requiredParam));
            }
        }
    }

    /**
     * Override method of any endpoint 'required' url parameters
     * @return list of required parameters
     */
    protected List<String> getRequiredParameters() {
        return Collections.emptyList();
    }
}
