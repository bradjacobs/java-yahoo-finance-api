/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.screener;

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
public class ScreenerCriteria
{
    @JsonProperty("size")
    private Integer size;
    @JsonProperty("offset")
    private Integer offset;
    @JsonProperty("sortField")
    private String sortField;
    @JsonProperty("sortType")
    private String sortType;
    @JsonProperty("quoteType")
    private String quoteType;
    @JsonProperty("topOperator")
    private String topOperator;
    @JsonProperty("query")
    private Query query;
    @JsonProperty("userId")
    private String userId = "";
    @JsonProperty("userIdType")
    private String userIdType = "";

    public Integer getSize()
    {
        return size;
    }

    public void setSize(Integer size)
    {
        this.size = size;
    }

    public Integer getOffset()
    {
        return offset;
    }

    public void setOffset(Integer offset)
    {
        this.offset = offset;
    }

    public String getSortField()
    {
        return sortField;
    }

    public void setSortField(String sortField)
    {
        this.sortField = sortField;
    }

    public String getSortType()
    {
        return sortType;
    }

    public void setSortType(String sortType)
    {
        this.sortType = sortType;
    }

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

    public Query getQuery()
    {
        return query;
    }

    public void setQuery(Query query)
    {
        this.query = query;
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
