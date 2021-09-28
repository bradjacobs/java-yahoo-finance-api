/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.response.converter.adapter;

import com.github.bradjacobs.yahoofinance.response.converter.ResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.util.JsonNestedFormatRemover;

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
public class JsonNestedFormatRemoverAdapter implements ResponseConverter
{
    private final ResponseConverter targetConverter;
    private final JsonNestedFormatRemover jsonNestedFormatRemover;

    /**
     *
     * @param targetConverter nested converter
     * @param removeEmptyEntries (true = remove empty entries, false = reassign value to 'null')
     */
    public JsonNestedFormatRemoverAdapter(ResponseConverter targetConverter, boolean removeEmptyEntries)
    {
        if (targetConverter == null) {
            throw new IllegalArgumentException("Must provide a target response converter.");
        }

        this.targetConverter = targetConverter;
        this.jsonNestedFormatRemover = new JsonNestedFormatRemover(removeEmptyEntries);
    }


    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConverter.convertToListOfMaps( jsonNestedFormatRemover.removeFormats(json) );
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return targetConverter.convertToMapOfMaps( jsonNestedFormatRemover.removeFormats(json) );
    }

}
