/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.response.converter.experiment.decorator;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.bradjacobs.yahoofinance.util.JsonMapperSingleton;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Given a JSON structure, return JSON with the "raw" values flattened.
 *
 * NOTE: this is just like the behavior of yahoo param
 *     "...&formatted=false"
 *
 * HOWEVER
 *    the need for this arose b/c the 'formatted' yahoo param does _NOT_ work in all cases.
 *       (i.e.  ".../quoteSummary/__ticker__?modules=balanceSheetHistory&formatted=false"
 *
 * Behavior Example:
 *   INPUT
 *       ...
 *     "enterpriseValue": { "raw": 2216864251904, "fmt": "2.22T", "longFmt": "2,216,864,251,904" },
 *     "totalAssets": { },
 *     "forwardPE": { "raw": 24.466228, "fmt": "24.47" },
 *
 *  OUTPUT ( removeEmptyEntries = TRUE )
 *       ...
 *     "enterpriseValue": 2216864251904,
 *     "forwardPE": 24.466228,
 *
 *  OUTPUT ( removeEmptyEntries = FALSE )
 *       ...
 *     "enterpriseValue": 2216864251904,
 *     "totalAssets": null,
 *     "forwardPE": 24.466228,
 */
public class JsonNestedFormatRemoverDecorator implements ResponseConverter
{
    private static final JsonMapper mapper = JsonMapperSingleton.getInstance();

    private static final String RAW_KEY = "raw";

    private final ResponseConverter targetConveter;

    //      * @param removeEmptyEntries  (true = remove empty entries, false = reassign value to 'null')
    private final boolean removeEmptyEntries;

    public JsonNestedFormatRemoverDecorator(ResponseConverter targetResponseConveter, boolean removeEmptyEntries) {
        if (targetResponseConveter == null) {
            throw new IllegalArgumentException("Must provide a target resposne convergter.");
        }

        this.targetConveter = targetResponseConveter;
        this.removeEmptyEntries = removeEmptyEntries;
    }


    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConveter.convertToListOfMaps( removeFormats(json) );
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return targetConveter.convertToMapOfMaps( removeFormats(json) );
    }



    /**
     * Removes the special Yahoo fmt sub-fields and replaces with 'raw' value.
     *   Behaves like "&formatted=false"
     * @param json json
     * @return the modified json
     */
    public String removeFormats(String json)
    {
        JsonNode node = covertToNode(json);
        removeFormats(node);

        try {
            return mapper.writeValueAsString(node);
        }
        catch (JsonProcessingException e) {
            // this 'should' never happen
            throw new IllegalArgumentException("Unable to transform node back to json string: " + e.getMessage(), e);
        }
    }

    /**
     * Side-effect the node removing format sections (if applicable)
     * @param node node
     */
    public void removeFormats(JsonNode node)
    {
        // recurse thru entire node tree, updating as needed.
        if (node.isArray()) {
            for (JsonNode childNode : node) {
                removeFormats(childNode);
            }
        }
        else if (node.isObject())
        {
            ObjectNode objNode = (ObjectNode) node;

            // keep track if there are any object fields that don't have a value
            List<String> emptyObjectFieldNames = new ArrayList<>();
            boolean rawValuesFound = false;

            for (Iterator<Map.Entry<String, JsonNode>> it = node.fields(); it.hasNext(); )
            {
                Map.Entry<String, JsonNode> entry = it.next();
                String fieldName = entry.getKey();
                JsonNode childNode = entry.getValue();

                if (childNode.isObject() && childNode.size() == 0) {
                    emptyObjectFieldNames.add(fieldName);
                }

                JsonNode rawValueNode = childNode.get(RAW_KEY);
                if (rawValueNode != null) {
                    // if found a 'raw' value, the reassign the parent to the true value
                    rawValuesFound = true;
                    objNode.set(fieldName, rawValueNode);
                }
                else {
                    // if not found, continue the recursion.
                    removeFormats(childNode);
                }
            }

            // if any raw fields were found
            //   _AND_
            // if there were any empty sibling fields,
            //   then update the empty siblings accordingly
            if (rawValuesFound && emptyObjectFieldNames.size() > 0) {
                for (String fieldName : emptyObjectFieldNames) {
                    if (removeEmptyEntries) {
                        objNode.remove(fieldName); // remove entirely
                    }
                    else {
                        objNode.set(fieldName, null); // field get a 'null' value
                    }
                }
            }
        }
    }

    private JsonNode covertToNode(String json)
    {
        if (StringUtils.isEmpty(json)) {
            throw new IllegalArgumentException("Must provide json data");
        }

        try {
            return mapper.readTree(json);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON string: " + e.getMessage(), e);
        }
    }

}
