/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.screener;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
//@JsonPropertyOrder({
//    "size",
//    "offset",
//    "sortField",
//    "sortType",
//    "entityIdType",
//    "includeFields",
//    "query"
//})
@JsonPropertyOrder({
        "sortType",
        "entityIdType",
        "sortField",
        "includeFields",
        "query",
        "offset",
        "size"
})
public class VisualizationCriteria extends AbstractRequestCriteria
{
    @JsonProperty("entityIdType")
    private String entityIdType;

    @JsonProperty("includeFields")
    private List<String> includeFields;

    public List<String> getIncludeFields() {
        return includeFields;
    }

    public void setIncludeFields(List<String> includeFields) {
        this.includeFields = includeFields;
    }

    public String getEntityIdType() {
        return entityIdType;
    }

    public void setEntityIdType(String entityIdType) {
        this.entityIdType = entityIdType;
    }

}
