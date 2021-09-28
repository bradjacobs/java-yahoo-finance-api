/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

class EnumInfo implements Comparable<EnumInfo>
{
    private final String enumName;
    private final List<EnumParamInfo> enumParamValues = new ArrayList<>();

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

        enumName = enumName.replace(",", "");

        // this special '&' case must come BEFORE the whitespace cleanup.
        enumName = enumName.replace("&", " AND ");

        // replace 2 or more adjacent spaces w/ a single space (i.e. "aaa   bbb" -> "aaa bbb")
        enumName = enumName.replaceAll("\\s+", " ");

        enumName = enumName.replace(" ", "_");
        enumName = enumName.replace("-", "_");  // primary dash
        enumName = enumName.replace("—", "_");  // the 'other' dash

        return enumName;
    }

    @Override
    public int compareTo(EnumInfo o)
    {
        return this.enumName.compareTo(o.enumName);
    }


    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof EnumInfo))
        {
            return false;
        }
        EnumInfo enumInfo = (EnumInfo) o;
        return enumName.equals(enumInfo.enumName) &&
            Objects.equals(enumParamValues, enumInfo.enumParamValues);
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(enumName, enumParamValues);
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
