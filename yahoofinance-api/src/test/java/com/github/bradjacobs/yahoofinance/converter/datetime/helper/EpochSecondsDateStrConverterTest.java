/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsDateStrConverter;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsDateStrConverterTest
{
    private final EpochSecondsDateStrConverter converter = new EpochSecondsDateStrConverter();

    @Test
    public void testEpochToDateString() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608595200L;
        String expected = "2020-12-22";
        assertEquals(converter.convertToString(inputDate), expected);
    }

    @Test
    public void testDateTimeStringToEpochSeconds() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        String input = "2020-12-22";
        Long expected = 1608595200L;
        assertEquals(converter.convertToEpoch(input), expected);
    }


    @Test
    public void testEpochToDateStringExtraHoursMinutes() throws Exception
    {
        // 1608677748  -- > GMT: Tuesday, December 22, 2020 10:55:48 PM
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608677748L;
        String expected = "2020-12-22";
        assertEquals(converter.convertToString(inputDate), expected);
    }


    @Test
    public void testNullDateString() throws Exception
    {
        assertNull(converter.convertToEpoch(null));
    }

    @Test
    public void testNullEpochSeconds() throws Exception
    {
        assertNull(converter.convertToString(null));
    }
}
