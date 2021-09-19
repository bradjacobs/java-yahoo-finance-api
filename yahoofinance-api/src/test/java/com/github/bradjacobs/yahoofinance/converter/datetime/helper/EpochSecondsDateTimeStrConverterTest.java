/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsDateTimeStrConverterTest
{
    private final EpochSecondsDateTimeStringConverter converter = new EpochSecondsDateTimeStringConverter();

    @Test
    public void testEpochToDateTimeString() throws Exception
    {
        // 1608677748  -- > GMT: Tuesday, December 22, 2020 10:55:48 PM
        Long inputDate = 1608677748L;
        String expected = "2020-12-22 22:55";

        // note: currently only minute granulatrity, so seconds are lost.
        assertEquals(converter.convertFromEpochSeconds(inputDate), expected);
    }

    @Test
    public void testDateTimeStringToEpochSeconds() throws Exception
    {
        // note: input string only has minute granularity

        // 1608677700  -- > GMT: Tuesday, December 22, 2020 10:55:00 PM
        String input = "2020-12-22 22:55";
        Long expected = 1608677700L;
        assertEquals(converter.convertFromObject(input), expected);
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
