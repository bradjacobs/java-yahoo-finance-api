/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter;

import bwj.yahoofinance.model.data.PriceHistoryRecord;
import bwj.yahoofinance.util.ChartDataConverter;
import org.testng.annotations.Test;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.*;

public class ChartDataConverterTest
{
    @Test
    public void testConvertToCollection() throws Exception
    {
        String originalJson = readTestResourceFile("aapl_chart_5d_formatted.json");

        ChartDataConverter chartDataConverter = new ChartDataConverter();
        List<Map<String, Number>> listOfMapRecords = chartDataConverter.toListOfMaps(originalJson);

        assertNotNull(listOfMapRecords);
        assertEquals(listOfMapRecords.size(), 5);

        List<PriceHistoryRecord> recordList = chartDataConverter.toRecordList(originalJson);

        assertNotNull(recordList);
        assertEquals(recordList.size(), 5);
    }


    private String readTestResourceFile(String fileName)
    {
        try {
            URL resource = getClass().getClassLoader().getResource(fileName);
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read test resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }

}
