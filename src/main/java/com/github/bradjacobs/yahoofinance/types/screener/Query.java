/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.screener;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "operator",
    "operands"
})
public class Query {

    @JsonProperty("operator")
    private String operator;
    @JsonProperty("operands")
    private List<Operand> operands = null;

    public String getOperator()
    {
        return operator;
    }

    public void setOperator(String operator)
    {
        this.operator = operator;
    }

    public List<Operand> getOperands()
    {
        return operands;
    }

    public void setOperands(List<Operand> operands)
    {
        this.operands = operands;
    }

}
