package com.github.bradjacobs.yahoofinance.response.converter;

import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class JsonPathCollectionConverter implements ResponseConverter
{
    private final String listOfObjectsPath;
    private final String mapOfObjectsPath;
    private final JsonPathDocContextCreator jsonPathDocContextCreator;


    public JsonPathCollectionConverter(String listOfObjectsPath, String mapOfObjectsPath) {
        this.listOfObjectsPath = listOfObjectsPath;
        this.mapOfObjectsPath = mapOfObjectsPath;
        this.jsonPathDocContextCreator = new JsonPathDocContextCreator();
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {

        if (StringUtils.isEmpty(this.listOfObjectsPath)) {
            throw new IllegalStateException("Path for List of Maps is not configured.");
        }
        DocumentContext jsonDoc = jsonPathDocContextCreator.createDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyList();
        }
        return jsonDoc.read(this.listOfObjectsPath);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {

        if (StringUtils.isEmpty(this.mapOfObjectsPath)) {
            throw new IllegalStateException("Path for Map of Maps is not configured.");
        }

        DocumentContext jsonDoc = jsonPathDocContextCreator.createDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyMap();
        }
        return jsonDoc.read(this.mapOfObjectsPath);
    }
}
