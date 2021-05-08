/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request.builder;

import org.apache.commons.lang.StringUtils;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Class for common code for _ALL_ requests regarding building the parameter map.
 * @param <T>
 */
abstract public class BaseRequestParamMapBuilder<T extends BaseRequestParamMapBuilder<T>>
{

    private boolean includeRegionParam = true;
    private String region = Locale.getDefault().getCountry();

    // for any other misc params
    private Map<String,String> extraParametersMap = new LinkedHashMap<>();



    protected abstract T getThis();


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

    public T withRegion(String region) {
        if (StringUtils.isEmpty(region)) {
            throw new IllegalArgumentException("Region value cannot be blank/empty.");
        }
        this.region = region;
        return getThis();
    }

    public T addParam(String key, String value) {
        if (key != null) {
            key = key.trim();
            if (value != null) {
                this.extraParametersMap.put(key, value.trim());
            }
            else {
                this.extraParametersMap.remove(key);
            }
        }
        return getThis();
    }

    protected Map<String,String> buildParamMap() {

        Map<String, String> paramMap = buildRequestSpecificMap();
        paramMap.putAll(this.extraParametersMap);

        if (includeRegionParam) {
            paramMap.put(ParamKeys.REGION, this.region);
        }
        return paramMap;
    }

    abstract protected Map<String,String> buildRequestSpecificMap();




}
