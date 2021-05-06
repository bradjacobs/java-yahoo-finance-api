/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.types.screener;

public enum Operator
{
    EQUAL("eq"),
    LESS_THAN("lt"),
    GREATER_THAN("gt"),
    BETWEEN("btwn"),

    AND("AND"),
    OR("OR");

    private final String value;

    Operator(String value)
    {
        this.value = value;
    }

    public String getValue()
    {
        return value;
    }

    @Override
    public String toString()
    {
        return value;
    }
}
