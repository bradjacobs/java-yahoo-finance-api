/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import java.util.ArrayList;
import java.util.List;

class EnumInfo implements Comparable<EnumInfo>
{
    private String enumName;
    private List<EnumParamInfo> enumParamValues = new ArrayList<>();

    public EnumInfo(String enumName)
    {
        this.enumName = enumName;
    }

    public void addParamValue(String value) {
        addParamValue(value, true);
    }
    public void addParamValue(String value, boolean isString) {
        this.enumParamValues.add(new EnumParamInfo(value, isString));
    }

    public String getEnumName()
    {
        return enumName;
    }

    public List<EnumParamInfo> getEnumParamValues()
    {
        return enumParamValues;
    }

    public static String makeEnumStyleFriendly(String enumName)
    {
        enumName = enumName.toUpperCase();
        enumName = enumName.replace(" ", "_");
        enumName = enumName.replace("-", "_");
        return enumName;
    }

    @Override
    public int compareTo(EnumInfo o)
    {
        return this.enumName.compareTo(o.enumName);
    }

    static class EnumParamInfo {
        private final String paramValue;
        private final boolean isString;

        public EnumParamInfo(String paramValue) {
           this(paramValue, true);
        }
        public EnumParamInfo(String paramValue, boolean isString) {
            this.paramValue = paramValue;
            this.isString = isString;
        }

        public String getParamValue() {
            return paramValue;
        }

        public boolean isString() {
            return isString;
        }
    }

}
