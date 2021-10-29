package com.github.bradjacobs.yahoofinance.response.converter.util;

import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.DocumentContext;

import java.util.Collections;
import java.util.List;

// todo - class needs better name & location
public class TestJsonPathCollectionConverter
{
    public List<Object> extractListOfObjects(String json, String path) {

        JsonPathDocContextCreator jsonPathDocContextCreator = new JsonPathDocContextCreator();
        DocumentContext jsonDoc = jsonPathDocContextCreator.createDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyList();
        }
        return jsonDoc.read(path);
    }
}
