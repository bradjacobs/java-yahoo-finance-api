
package com.github.bradjacobs.yahoofinance.model.beta;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/*
NOTE:  this class was generated using the response from the following request:
   https://query1.finance.yahoo.com/v1/finance/search?q=ba*&quotesCount=2&newsCount=2

   still trying to understand how it works.
 */

/**
 * Basic POJO for response of the search endpoint.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SearchResultBeta
{
    private List<Object> explains = null;
    private Integer count;

    @JsonProperty("quotes")
    private List<QuoteItem> quoteItems = null;

    @JsonProperty("news")
    private List<NewsItem> newsItems = null;

    @JsonProperty("nav")
    private List<NavItem> navItems = null;

    @JsonProperty("lists")
    private List<ListItem> listItems = null;

    @JsonProperty("researchReports")
    private List<Object> researchReports = null;


    private Integer totalTime;
    private Integer timeTakenForQuotes;
    private Integer timeTakenForNews;
    private Integer timeTakenForAlgowatchlist;
    private Integer timeTakenForPredefinedScreener;
    private Integer timeTakenForCrunchbase;
    private Integer timeTakenForNav;
    private Integer timeTakenForResearchReports;

    public List<Object> getExplains()
    {
        return explains;
    }

    public void setExplains(List<Object> explains)
    {
        this.explains = explains;
    }

    public Integer getCount()
    {
        return count;
    }

    public void setCount(Integer count)
    {
        this.count = count;
    }

    public List<QuoteItem> getQuoteItems()
    {
        return quoteItems;
    }

    public void setQuoteItems(List<QuoteItem> quoteItems)
    {
        this.quoteItems = quoteItems;
    }

    public List<NewsItem> getNewsItems()
    {
        return newsItems;
    }

    public void setNewsItems(List<NewsItem> newsItems)
    {
        this.newsItems = newsItems;
    }

    public List<NavItem> getNavItems()
    {
        return navItems;
    }

    public void setNavItems(List<NavItem> navItems)
    {
        this.navItems = navItems;
    }

    public List<ListItem> getListItems()
    {
        return listItems;
    }

    public void setListItems(List<ListItem> listItems)
    {
        this.listItems = listItems;
    }

    public List<Object> getResearchReports()
    {
        return researchReports;
    }

    public void setResearchReports(List<Object> researchReports)
    {
        this.researchReports = researchReports;
    }

    public Integer getTotalTime()
    {
        return totalTime;
    }

    public void setTotalTime(Integer totalTime)
    {
        this.totalTime = totalTime;
    }

    public Integer getTimeTakenForQuotes()
    {
        return timeTakenForQuotes;
    }

    public void setTimeTakenForQuotes(Integer timeTakenForQuotes)
    {
        this.timeTakenForQuotes = timeTakenForQuotes;
    }

    public Integer getTimeTakenForNews()
    {
        return timeTakenForNews;
    }

    public void setTimeTakenForNews(Integer timeTakenForNews)
    {
        this.timeTakenForNews = timeTakenForNews;
    }

    public Integer getTimeTakenForAlgowatchlist()
    {
        return timeTakenForAlgowatchlist;
    }

    public void setTimeTakenForAlgowatchlist(Integer timeTakenForAlgowatchlist)
    {
        this.timeTakenForAlgowatchlist = timeTakenForAlgowatchlist;
    }

    public Integer getTimeTakenForPredefinedScreener()
    {
        return timeTakenForPredefinedScreener;
    }

    public void setTimeTakenForPredefinedScreener(Integer timeTakenForPredefinedScreener)
    {
        this.timeTakenForPredefinedScreener = timeTakenForPredefinedScreener;
    }

    public Integer getTimeTakenForCrunchbase()
    {
        return timeTakenForCrunchbase;
    }

    public void setTimeTakenForCrunchbase(Integer timeTakenForCrunchbase)
    {
        this.timeTakenForCrunchbase = timeTakenForCrunchbase;
    }

    public Integer getTimeTakenForNav()
    {
        return timeTakenForNav;
    }

    public void setTimeTakenForNav(Integer timeTakenForNav)
    {
        this.timeTakenForNav = timeTakenForNav;
    }

    public Integer getTimeTakenForResearchReports()
    {
        return timeTakenForResearchReports;
    }

    public void setTimeTakenForResearchReports(Integer timeTakenForResearchReports)
    {
        this.timeTakenForResearchReports = timeTakenForResearchReports;
    }



    @JsonInclude(JsonInclude.Include.NON_NULL)
    static class QuoteItem
    {
        public String exchange;
        public String shortname;
        public String quoteType;
        public String symbol;
        public String index;
        public Float score;
        public String typeDisp;
        public String longname;
        public Boolean isYahooFinance;
    }


    @JsonInclude(JsonInclude.Include.NON_NULL)
    static class NewsItem
    {
        public String uuid;
        public String title;
        public String publisher;
        public String link;
        public Integer providerPublishTime;
        public String type;
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    static class NavItem
    {
        @JsonProperty("navName")
        public String navName;

        @JsonProperty("navUrl")
        public String navUrl;
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    static class ListItem
    {
        public String slug;
        public String name;
        public String index;
        public Float score;
        public String type;
        public String brandSlug;
        public String pfId;
        public String id;
        public String title;
        public String canonicalName;
    }
}
