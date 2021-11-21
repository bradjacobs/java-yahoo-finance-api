package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.response.converter.util.SimpleMapOfMapsGenerator;
import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class VisualizationResponseConverter implements ResponseConverter
{
    private static final String DOCUMENTS_PATH = "$.finance.result[0].documents[0]";
    private static final String COLUMN_NAMES_PATH = DOCUMENTS_PATH + ".columns[*].id";
    private static final String DATA_PATH = DOCUMENTS_PATH + ".rows[*]";

    private static final String MAP_KEY = "ticker";

    private static final JsonPathDocContextCreator JSON_PATH_DOC_CONTEXT_CREATOR = new JsonPathDocContextCreator();
    private final SimpleMapOfMapsGenerator mapOfMapsGenerator = new SimpleMapOfMapsGenerator(MAP_KEY, true);

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return mapOfMapsGenerator.convertToMap( this.convertToListOfMaps(json) );
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        List<Map<String,Object>> resultListOfMaps = new ArrayList<>();

        DocumentContext jsonDoc = JSON_PATH_DOC_CONTEXT_CREATOR.createDocContext(json);
        String[] columnNames = jsonDoc.read(COLUMN_NAMES_PATH, String[].class);
        int columnCount = columnNames.length;

        List<List<Object>>  listOfEntryResults = jsonDoc.read(DATA_PATH);

        for (List<Object> elementValueList : listOfEntryResults) {

            Map<String,Object> elementEntryMap = new HashMap<>();
            for (int i = 0; i < columnCount; i++) {
                elementEntryMap.put(columnNames[i], elementValueList.get(i));
            }
            resultListOfMaps.add(elementEntryMap);
        }

        return resultListOfMaps;
    }
}
