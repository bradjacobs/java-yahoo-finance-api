/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Given a JSON structure, return JSON with the "raw" values flattened.
 *
 * NOTE: this is like the behavior of yahoo param "...&formatted=false"
 *   BUT this yahoo param does _NOT_ work in all cases.
 *     (i.e.  ".../quoteSummary/__ticker__?modules=balanceSheetHistory&formatted=false"
 *
 * Example:
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
public class JsonFormatRemover
{
    private static final ObjectMapper mapper = new ObjectMapper();

    /**
     * Removes the special Yahoo fmt sub-fields and replaces with 'raw' value.
     *   Behaves like "&formatted=false"
     * @param json json
     * @param removeEmptyEntries  (true = remove empty entries, false = reassign value to 'null')
     * @return the modified json
     */
    public static String removeFormats(String json, boolean removeEmptyEntries)
    {
        JsonNode node = covertToNode(json);
        removeFormats(node, removeEmptyEntries);

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
     * @param removeEmptyEntries (true = remove empty entries, false = reassign value to 'null')
     */
    public static void removeFormats(JsonNode node, boolean removeEmptyEntries)
    {
        // recurse thru entire node tree, updating as needed.
        if (node.isArray()) {
            for (JsonNode childNode : node) {
                removeFormats(childNode, removeEmptyEntries);
            }
        }
        else if (node.isObject())
        {
            ObjectNode objNode = (ObjectNode) node;

            // keep track of any object fields that don't have a value
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

                JsonNode rawValueNode = childNode.get("raw");
                if (rawValueNode != null) {
                    // if found a 'raw' value, the reassign the parent to the true value
                    rawValuesFound = true;
                    objNode.set(fieldName, rawValueNode);
                }
                else {
                    removeFormats(childNode, removeEmptyEntries);
                }
            }

            // if any raw fields were found AND if there were any empty sibling fields,
            //   then update them accordingly
            if (rawValuesFound && emptyObjectFieldNames.size() > 0) {
                for (String fieldName : emptyObjectFieldNames) {
                    if (removeEmptyEntries) {
                        objNode.remove(fieldName);
                    }
                    else {
                        objNode.set(fieldName, null);
                    }
                }
            }
        }
    }

    private static JsonNode covertToNode(String json)
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