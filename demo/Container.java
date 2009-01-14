class Container {
  public int tag;
  public Object[] payload;
  public int intVal;
  public String stringVal;

  static void alloc (int size)
  {
    Container c = new Container();
    c.payload = new Object[size - 1];
    c.payload[3] = c;
  }

}


