/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types;

import java.util.HashMap;
import java.util.Map;

/**
 * Figure out a good enum name when dealing with a time interval value
 *   i.e.  "5d"   ->  "FIVE_DAYS"
 */
class TimeIntervalEnumInfoGenerator
{
    private static final Map<String,String> suffixNameMap = new HashMap<String,String>() {{
        put("m", "MIN");
        put("h", "HOUR");
        put("d", "DAY");
        put("wk", "WEEK");
        put("mo", "MONTH");
        put("y", "YEAR");
    }};

    // note: 'assuming' all number choices will be one of the following
    private static final Map<Integer,String> numberNameMap = new HashMap<Integer,String>() {{
        put(1, "ONE");
        put(2, "TWO");
        put(3, "THREE");
        put(4, "FOUR");
        put(5, "FIVE");
        put(6, "SIX");
        put(10, "TEN");
        put(12, "TWELVE");
        put(15, "FIFTEEN");
        put(20, "TWENTY");
        put(30, "THIRTY");
        put(60, "SIXTY");
        put(90, "NINETY");
    }};

    public static EnumInfo generateEnumInfo(String value)
    {
        EnumInfo enumInfo = null;
        if (containsNumber(value))
        {
            for (Map.Entry<String, String> entry : suffixNameMap.entrySet())
            {
                String suffix = entry.getKey();
                if (value.endsWith(suffix)) {
                    String numberString = value.replace(suffix, "");

                    try {
                        int numberVal = Integer.parseInt(numberString);
                        String numberName = numberNameMap.get(numberVal);
                        String suffixUnitName = entry.getValue();
                        if (numberName != null)
                        {
                            // some reason putting the 's' on min seems to look funny (probably just me)
                            if (numberVal != 1 && !suffixUnitName.equals("MIN"))
                            {
                                suffixUnitName += "S";
                            }
                            String formalName = numberName + "_" + suffixUnitName;
                            enumInfo = new EnumInfo(formalName);
                        }
                    }
                    catch (Exception e) {
                        // ignore
                    }
                }
            }
        }

        if (enumInfo == null) {
            enumInfo = new EnumInfo( EnumInfo.makeEnumStyleFriendly(value));
        }

        enumInfo.addParamValue(value);
        return enumInfo;
    }

    private static boolean containsNumber(String value) {
        return value.matches(".*\\d.*");
    }

}
