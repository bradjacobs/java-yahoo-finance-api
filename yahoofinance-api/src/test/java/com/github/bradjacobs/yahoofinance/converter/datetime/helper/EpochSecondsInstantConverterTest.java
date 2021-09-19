/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.helper.EpochSecondsInstantConverter;
import org.testng.annotations.Test;

import java.time.Instant;

import static org.testng.Assert.assertEquals;
import static org.testng.AssertJUnit.assertNull;

public class EpochSecondsInstantConverterTest
{
    private final EpochSecondsInstantConverter converter = new EpochSecondsInstantConverter();

    @Test
    public void testInstantToEpochSeconds() throws Exception
    {
        Instant instant = Instant.now();
        Long expected = instant.getEpochSecond();
        assertEquals(converter.convertToEpoch(instant), expected);
    }

    @Test
    public void testEpochSecondsToInstant() throws Exception
    {
        Long inputSeconds = 1602077748L;
        Instant result = converter.convertToInstant(inputSeconds);
        assertEquals((Long)result.getEpochSecond(), inputSeconds);
    }

    @Test
    public void testNullInstant() throws Exception
    {
        assertNull(converter.convertToEpoch(null));
    }

    @Test
    public void testNullEpoch() throws Exception
    {
        assertNull(converter.convertToInstant(null));
    }
}
