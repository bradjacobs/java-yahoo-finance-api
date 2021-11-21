/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.criteria;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "size",
    "offset",
    "sortField",
    "sortType",
    "quoteType",
    "topOperator",
    "query",
    "userId",
    "userIdType"
})
public class ScreenerCriteria extends AbstractRequestCriteria
{
    @JsonProperty("quoteType")
    private String quoteType;
    @JsonProperty("topOperator")
    private String topOperator;
    @JsonProperty("userId")
    private String userId = "";
    @JsonProperty("userIdType")
    private String userIdType = "";

    public String getQuoteType()
    {
        return quoteType;
    }

    public void setQuoteType(String quoteType)
    {
        this.quoteType = quoteType;
    }

    public String getTopOperator()
    {
        return topOperator;
    }

    public void setTopOperator(String topOperator)
    {
        this.topOperator = topOperator;
    }

    public String getUserId()
    {
        return userId;
    }

    public void setUserId(String userId)
    {
        this.userId = userId;
    }

    public String getUserIdType()
    {
        return userIdType;
    }

    public void setUserIdType(String userIdType)
    {
        this.userIdType = userIdType;
    }
}
