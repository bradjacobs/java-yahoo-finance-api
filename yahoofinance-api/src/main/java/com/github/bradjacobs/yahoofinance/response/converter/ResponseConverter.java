package com.github.bradjacobs.yahoofinance.response.converter;

import java.util.List;
import java.util.Map;

public interface ResponseConverter
{
    List<Map<String,Object>> convertToListOfMaps(String json);

    Map<String,Map<String,Object>> convertToMapOfMaps(String json);

}
