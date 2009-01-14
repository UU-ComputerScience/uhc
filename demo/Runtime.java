class Runtime
{
  public static class Container
  {
    public int tag;
    public Object[] payload;
    public int intVal;
    public String stringVal;

    public Container (int tag, Object[] payload)
    {
      this.tag     = tag;
      this.payload = payload;
    }
  }

  // Todo: parametrize these from haskell.
  public static Object[]        RP = new Object[256];
  public static Container      CRP = new Container(0, Runtime.RP);
  public static Object[]   Globals = new Object[256];
  public static Container CGlobals = new Container(0, Runtime.Globals);

  public static void finish ()
  {
    System.out.println(Runtime.CRP.intVal);
  }

  static void test (int size)
  {
    Container c = new Container(0, null);
    c.payload = new Object[size - 1];
    c.payload[3] = c;
    System.out.println(c.payload[2]);
    System.out.println(Runtime.RP);
  }

}

