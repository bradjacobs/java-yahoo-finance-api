/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.criteria;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.List;

/**
 *  NOTE:  Having a List of 'Objects' is kinda lame.
 *    but the operands can be of different types + the ROI of making
 *    custom serialization/deserialization really isn't there at present.
 *
 *    ex1:  "operands": [ "fiftytwowkpercentchange", 12.5 ]
 *    ex2:  "operands": [ "sector", "Financial Services" ]
 *    ex3:  "operands": [ { "operator": "EQ", "operands": ["region","us"] } ]
 *
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "operator",
    "operands"
})
public class Operand
{
    @JsonProperty("operator")
    private String operator;

    @JsonProperty("operands")
    private List<Object> operands;

    public String getOperator()
    {
        return operator;
    }

    public void setOperator(String operator)
    {
        this.operator = operator;
    }

    public List<Object> getOperands()
    {
        return operands;
    }

    public void setOperands(List<Object> operands)
    {
        this.operands = operands;
    }
}
