/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime2;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import org.testng.annotations.Test;

import java.time.Instant;
import java.time.LocalDate;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsConverterTest
{
    private final EpochSecondsConverter converter = new EpochSecondsConverter();

    @Test
    public void testEpochToDateString() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608595200L;
        String expected = "2020-12-22";
        assertEquals(converter.toString(inputDate), expected);
    }

    @Test
    public void testDateTimeStringToEpochSeconds() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        String input = "2020-12-22";
        Long expected = 1608595200L;
        assertEquals(converter.fromString(input), expected);
    }

    @Test
    public void testEpochToDateStringExtraHoursMinutes() throws Exception
    {
        // 1608677748  -- > GMT: Tuesday, December 22, 2020 10:55:48 PM
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608677748L;
        String expected = "2020-12-22";
        assertEquals(converter.toString(inputDate), expected);
    }


    @Test
    public void testEpochToLocalDate() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long inputDate = 1608595200L;
        LocalDate expected = LocalDate.of(2020,12,22);

        LocalDate localDate = converter.toLocalDate(inputDate);
        assertNotNull(localDate);
        assertEquals(localDate, expected);
    }

    @Test
    public void testLocalDateToEpoch() throws Exception
    {
        // 1608595200  -- > GMT: Tuesday, December 22, 2020 12:00:00 AM
        Long expected = 1608595200L;
        LocalDate localDate = LocalDate.of(2020,12,22);

        Long result = converter.fromLocalDate(localDate);
        assertNotNull(result);
        assertEquals(result, expected);
    }

    @Test
    public void testInstantToEpochSeconds() throws Exception
    {
        Instant instant = Instant.now();
        Long expected = instant.getEpochSecond();
        assertEquals(converter.fromInstant(instant), expected);
    }

    @Test
    public void testEpochSecondsToInstant() throws Exception
    {
        Long inputSeconds = 1602077748L;
        Instant result = converter.toInstant(inputSeconds);
        assertEquals((Long)result.getEpochSecond(), inputSeconds);
    }


    @Test
    public void testNullDateString() throws Exception {
        assertNull(converter.fromString(null));
    }
    @Test
    public void testNullEpochSeconds() throws Exception {
        assertNull(converter.toString(null));
    }
}
