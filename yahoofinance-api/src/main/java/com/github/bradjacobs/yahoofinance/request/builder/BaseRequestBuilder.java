/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.Region;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.validation.YahooRequestValidator;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Class for common code for _ALL_ requests regarding building the parameter map.
 * @param <T>
 */
abstract public class BaseRequestBuilder<T extends BaseRequestBuilder<T>>
{
    protected static final YahooRequestValidator requestValidator = new YahooRequestValidator();

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

    protected Map<String,String> buildParamMap() {

        Map<String, String> paramMap = _buildParamMap();
        paramMap.putAll(this.extraParametersMap);

        if (includeRegionParam) {
            paramMap.put(ParamKeys.REGION, this.region);
        }
        return paramMap;
    }

    abstract protected Map<String,String> _buildParamMap();
    abstract protected YahooEndpoint _getRequestEndpoint();
    abstract protected String _getRequestTicker();

    protected Object _buildRequestPostBody() {
        return null;
    }

    public YahooFinanceRequest build() {

        YahooEndpoint endpoint = _getRequestEndpoint();
        String ticker = _getRequestTicker();
        Map<String, String> paramMap = buildParamMap();
        Object postBody = _buildRequestPostBody();

        YahooFinanceRequest req = generateRequest(endpoint, ticker, paramMap, postBody, additionalHeaderMap);
        validateRequest(req);
        return req;
    }

    protected YahooFinanceRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        return new YahooFinanceRequest(endpoint, ticker, paramMap, postBody, headerMap);
    }


    /**
     * Will throw exception if request is invalid
     * @param req request to be validated
     */
    protected void validateRequest(YahooFinanceRequest req)
    {
        // this will throw exception if request is invalid
        requestValidator.validationRequest(req);
    }
}
