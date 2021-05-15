/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.tools.internal.generator.types.autogen;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "displayName",
    "criteria"
})
public class Label {

    @JsonProperty("displayName")
    private String displayName;
    @JsonProperty("criteria")
    private Criteria criteria;

    @JsonProperty("displayName")
    public String getDisplayName() {
        return displayName;
    }

    @JsonProperty("displayName")
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    @JsonProperty("criteria")
    public Criteria getCriteria() {
        return criteria;
    }

    @JsonProperty("criteria")
    public void setCriteria(Criteria criteria) {
        this.criteria = criteria;
    }

}
