/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.screener.CriteriaEnum;

public enum Exchange implements CriteriaEnum
{
    //  NOTE:  looks like 'ARCA' can't be used for queries!
    ARCA("PCX", "NYSE Arca"),     // note: Yahoo calls the value 'PSX', but it is NyseArca

    ASE("ASE", "NYSE American"),  // a.k.a.  AMEX
    NASDAQCM("NCM", "NASDAQ Capital Market"),
    NASDAQGM("NGM", "NASDAQ (Global Market)"),
    NASDAQGS("NMS", "NASDAQ (Global Select Market)"),
    NYSE("NYQ", "New York Stock Exchange"),
    PNK("PNK", "Other OTC");



    //BSE("BSE", "__unused__"),  // listed as Yahoo Exchange choice, probably once meant "Boston Stock Exchange", but now looks to be "Bombay Stock Exchange"?
    //NASDAQ("NAS", "NASDAQ"),   // listed as Yahoo Exchange choice, but never seems to match anything.
    //YHD("YHD", "Yahoo (UNUSED)");  // listed as Yahoo Exchange choice, but doesn't seem to have any value.


    private final String value;
    private final String name;

    Exchange(String value, String name) {
        this.value = value;
        this.name = name;
    }

    public String getValue() {
        return value;
    }

    public String getName() {
        return name;
    }

    @Override
    public String getCriteriaValue() {
        return value;
    }
}
