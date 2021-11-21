/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;

import javax.annotation.Generated;
import java.util.HashMap;
import java.util.Map;

/**
 * List of Regions available
 *
 * NOTE: this list was derived from YAHOO!'s website for
 *   what they use of a list of Region choices.
 * this is _NOT_ a master list of all possible regions
 *
 * For a more exhaustive list see see https://en.wikipedia.org/wiki/ISO_3166-2
 *
 */
@Generated(value="yahoo-finance-api-internal-tools", date="2021-11-05")
public enum Region implements CriteriaEnum
{
    ARGENTINA("AR", "Argentina"),
    AUSTRALIA("AU", "Australia"),
    AUSTRIA("AT", "Austria"),
    BELGIUM("BE", "Belgium"),
    BRAZIL("BR", "Brazil"),
    CANADA("CA", "Canada"),
    CHILE("CL", "Chile"),
    CHINA("CN", "China"),
    CZECH_REPUBLIC("CZ", "Czech Republic"),
    DENMARK("DK", "Denmark"),
    EGYPT("EG", "Egypt"),
    ESTONIA("EE", "Estonia"),
    FINLAND("FI", "Finland"),
    FRANCE("FR", "France"),
    GERMANY("DE", "Germany"),
    GREECE("GR", "Greece"),
    HONG_KONG("HK", "Hong Kong"),
    HUNGARY("HU", "Hungary"),
    ICELAND("IS", "Iceland"),
    INDIA("IN", "India"),
    INDONESIA("ID", "Indonesia"),
    IRELAND("IE", "Ireland"),
    ISRAEL("IL", "Israel"),
    ITALY("IT", "Italy"),
    JAPAN("JP", "Japan"),
    KUWAIT("KW", "Kuwait"),
    LATVIA("LV", "Latvia"),
    LITHUANIA("LT", "Lithuania"),
    MALAYSIA("MY", "Malaysia"),
    MEXICO("MX", "Mexico"),
    NETHERLANDS("NL", "Netherlands"),
    NEW_ZEALAND("NZ", "New Zealand"),
    NORWAY("NO", "Norway"),
    PAKISTAN("PK", "Pakistan"),
    PERU("PE", "Peru"),
    PHILIPPINES("PH", "Philippines"),
    POLAND("PL", "Poland"),
    PORTUGAL("PT", "Portugal"),
    QATAR("QA", "Qatar"),
    RUSSIA("RU", "Russia"),
    SAUDI_ARABIA("SA", "Saudi Arabia"),
    SINGAPORE("SG", "Singapore"),
    SOUTH_AFRICA("ZA", "South Africa"),
    SOUTH_KOREA("KR", "South Korea"),
    SPAIN("ES", "Spain"),
    SRI_LANKA("LK", "Sri Lanka"),
    SURINAME("SR", "Suriname"),
    SWEDEN("SE", "Sweden"),
    SWITZERLAND("CH", "Switzerland"),
    TAIWAN("TW", "Taiwan"),
    THAILAND("TH", "Thailand"),
    TURKEY("TR", "Turkey"),
    UNITED_KINGDOM("GB", "United Kingdom"),
    UNITED_STATES("US", "United States"),
    VENEZUELA("VE", "Venezuela"),
    VIETNAM("VN", "Vietnam");


    private final String code;
    private final String displayName;

    private static final Map<String,Region> codeRegionMap = new HashMap<>();

    static {
        for (Region region : Region.values()) {
            codeRegionMap.put(region.getCode(), region);
        }
    }

    Region(String code, String displayName)
    {
        this.code = code;
        this.displayName = displayName;
    }

    public String getCode()
    {
        return code;
    }

    public String getDisplayName()
    {
        return displayName;
    }

    public static Region getRegion(String code) {
        return codeRegionMap.get(code);
    }

    @Override
    public String getCriteriaValue()
    {
        return code.toLowerCase();
    }
}
