package bwj.yahoofinance;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class HelloWorldTest
{
    @Test
    public void testHelloWorld() throws Exception {
        HelloWorld hw = new HelloWorld();
        assertEquals(hw.getHelloWorld(), "Hello World");

    }
}