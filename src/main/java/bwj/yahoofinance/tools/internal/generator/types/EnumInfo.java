package bwj.yahoofinance.tools.internal.generator.types;

import java.util.ArrayList;
import java.util.List;

class EnumInfo implements Comparable<EnumInfo>
{
    private String enumName;
    private List<String> enumParamValues = new ArrayList<>();

    public EnumInfo(String enumName)
    {
        this.enumName = enumName;
    }

    public void addParamValue(String value) {
        this.enumParamValues.add(value);
    }

    public String getEnumName()
    {
        return enumName;
    }

    public List<String> getEnumParamValues()
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
}
