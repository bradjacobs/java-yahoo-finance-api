/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types.criteria;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

// it is _assumed_ that all crteria have these common fields
//   (if discover otherwise, this class subject to removal)
abstract public class AbstractRequestCriteria
{
    private static final String SORT_DESC = "DESC";
    private static final String SORT_ASC = "ASC";

    @JsonProperty("size")
    private Integer size;
    @JsonProperty("offset")
    private Integer offset;
    @JsonProperty("sortField")
    private String sortField;
    @JsonProperty("sortType")
    private String sortType = SORT_ASC; // default value
    @JsonProperty("query")
    private Query query;

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

    @JsonIgnore
    public void setIsSortDescending(boolean sortDescending) {
        this.sortType = (sortDescending ? SORT_DESC : SORT_ASC);
    }

    public String getSortType()
    {
        return sortType;
    }

    public Query getQuery()
    {
        return query;
    }

    public void setQuery(Query query)
    {
        this.query = query;
    }
}
