/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import org.testng.annotations.Test;

import java.util.Date;

import static org.testng.Assert.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsDateConverterTest
{
    private final EpochSecondsDateConverter converter = new EpochSecondsDateConverter();

    @Test
    public void testDateToEpochSeconds() throws Exception
    {
        Date d = new Date();
        Long expected = d.getTime() / 1000;
        assertEquals(converter.convertFromObject(d), expected);
    }

    @Test
    public void testEpochSecondsToDate() throws Exception
    {
        Long inputSeconds = 1602077748L;
        Date expectedDate = new Date(inputSeconds * 1000);
        assertEquals(converter.convertFromEpochSeconds(inputSeconds), expectedDate);
    }

    @Test
    public void testNullDate() throws Exception
    {
        assertNull(converter.convertFromObject(null));
    }

    @Test
    public void testNullEpoch() throws Exception
    {
        assertNull(converter.convertFromEpochSeconds(null));
    }
}
