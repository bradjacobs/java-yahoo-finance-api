package com.github.bradjacobs.yahoofinance.response.converter;

import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import static org.testng.Assert.*;

public class JsonPathCollectionConverterTest
{
    @Test
    public void testConvertToMap() throws Exception
    {
        String mapPath = "$.widget.window";
        JsonPathCollectionConverter jsonPathCollectionConverter
                = new JsonPathCollectionConverter(null, mapPath);

        Map<String, Map<String, Object>> mapOfMaps
                = jsonPathCollectionConverter.convertToMapOfMaps(SAMPLE_MAP_JSON);

        assertNotNull(mapOfMaps);
        assertEquals(mapOfMaps.size(), 4);

        Object titleObject = mapOfMaps.get("title");
        Object widthObject = mapOfMaps.get("width");

        assertNotNull(titleObject);
        assertNotNull(widthObject);

        if (titleObject instanceof String) {
            assertEquals((String)titleObject, "My Widget");
        }
        else {
            fail("Unexpected class for title: " + titleObject.getClass().getSimpleName());
        }

        // expect whole numbers to be a "LONG"  (not an Integer)
        if (widthObject instanceof Long) {
            assertEquals((Long)widthObject, Long.valueOf(450));
        }
        else {
            fail("Unexpected class for width: " + widthObject.getClass().getSimpleName());
        }
    }

    @Test
    public void testConvertToList() throws Exception
    {
        String listPath = "$.menu.items[*]";
        JsonPathCollectionConverter jsonPathCollectionConverter
                = new JsonPathCollectionConverter(listPath, null);

        List<Map<String, Object>> listOfMaps
                = jsonPathCollectionConverter.convertToListOfMaps(SAMPLE_LIST_JSON);

        assertNotNull(listOfMaps);
        assertEquals(listOfMaps.size(), 7);

        // pick item at certain index (make sure objects are in correct order)
        Map<String,Object> elementMap = listOfMaps.get(2);
        assertNotNull(elementMap);

        Object findObject = elementMap.get("id");

        if (findObject instanceof String) {
            assertEquals((String)findObject, "Find");
        }
        else {
            fail("Unexpected class for title: " + findObject.getClass().getSimpleName());
        }
    }


    private static final String SAMPLE_MAP_JSON = "{\"widget\": {\n" +
            "    \"debug\": \"on\",\n" +
            "    \"window\": {\n" +
            "        \"title\": \"My Widget\",\n" +
            "        \"name\": \"widget_window\",\n" +
            "        \"width\": 450,\n" +
            "        \"height\": 300\n" +
            "    },\n" +
            "    \"image\": { \n" +
            "        \"src\": \"Images/Star.png\",\n" +
            "        \"name\": \"star2b\"\n" +
            "    },\n" +
            "    \"text\": {\n" +
            "        \"data\": \"Click Me\",\n" +
            "        \"size\": 24\n" +
            "    }\n" +
            "}}";

    private static final String SAMPLE_LIST_JSON = "{\"menu\": {\n" +
            "    \"header\": \"SVG Viewer\",\n" +
            "    \"items\": [\n" +
            "        {\"id\": \"Open\"},\n" +
            "        {\"id\": \"OpenNew\", \"label\": \"Open New\"},\n" +
            "        {\"id\": \"Find\", \"label\": \"Find...\"},\n" +
            "        {\"id\": \"Copy\"},\n" +
            "        {\"id\": \"Save\"},\n" +
            "        {\"id\": \"SaveAs\", \"label\": \"Save As\"},\n" +
            "        {\"id\": \"Help\"}\n" +
            "    ]\n" +
            "}}";
}
