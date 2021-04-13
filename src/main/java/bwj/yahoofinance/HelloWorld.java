package bwj.yahoofinance;

public class HelloWorld
{
    public static void main(String[] args) {
        HelloWorld helloWorld = new HelloWorld();
        helloWorld.printHelloWorld();
    }

    public void printHelloWorld()
    {
        System.out.println(getHelloWorld());
    }

    public String getHelloWorld() {
        return "Hello World";
    }
}
