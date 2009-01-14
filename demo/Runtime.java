class Runtime
{
  public static Object[] RP = new Object[256];

  public static class Container
  {
    public int tag;
    public Object[] payload;
    public int intVal;
    public String stringVal;
  }

  static void test (int size)
  {
    Container c = new Container();
    c.payload = new Object[size - 1];
    c.payload[3] = c;
    System.out.println(c.payload[2]);
    System.out.println(Runtime.RP);
  }

}

