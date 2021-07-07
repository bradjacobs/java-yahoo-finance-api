package com.github.bradjacobs.yahoofinance.response;

import java.util.List;
import java.util.Map;

public interface YahooResponseConverter
{
    List<Map<String,Object>> convertToListOfMaps(String json);

    Map<String,Map<String,Object>> convertToMapOfMaps(String json);
}
