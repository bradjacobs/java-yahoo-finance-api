/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsDateStringConverterTest
{
    private final EpochSecondsDateStringConverter converter = new EpochSecondsDateStringConverter();

    @Test
    public void testEpochToDateString() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608595200L;
        String expected = "2020-12-22";
        assertEquals(converter.convertFromEpochSeconds(inputDate), expected);
    }

    @Test
    public void testDateTimeStringToEpochSeconds() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        String input = "2020-12-22";
        Long expected = 1608595200L;
        assertEquals(converter.convertFromObject(input), expected);
    }


    @Test
    public void testEpochToDateStringExtraHoursMinutes() throws Exception
    {
        // 1608677748  -- > GMT: Tuesday, December 22, 2020 10:55:48 PM
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608677748L;
        String expected = "2020-12-22";
        assertEquals(converter.convertFromEpochSeconds(inputDate), expected);
    }


    @Test
    public void testNullDateString() throws Exception
    {
        assertNull(converter.convertFromObject(null));
    }

    @Test
    public void testNullEpochSeconds() throws Exception
    {
        assertNull(converter.convertFromEpochSeconds(null));
    }
}
