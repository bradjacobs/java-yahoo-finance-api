
package bwj.yahoofinance.misc.screener.autogen;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "operator",
    "operands"
})
public class Criteria {

    @JsonProperty("operator")
    private String operator;
    @JsonProperty("operands")
    private List<String> operands = null;

    @JsonProperty("operator")
    public String getOperator() {
        return operator;
    }

    @JsonProperty("operator")
    public void setOperator(String operator) {
        this.operator = operator;
    }

    @JsonProperty("operands")
    public List<String> getOperands() {
        return operands;
    }

    @JsonProperty("operands")
    public void setOperands(List<String> operands) {
        this.operands = operands;
    }

}
