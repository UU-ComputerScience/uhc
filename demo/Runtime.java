class Runtime
{
  public static class Node
  {
    public int tag;
    public Node[] payload;

    // Empty constructor needed.
    public Node () {}

    public Node (int tag, Node[] payload)
    {
      this.tag     = tag;
      this.payload = payload;
    }
  }

  public static class IntNode extends Node
  {
    public int intVal;
    public IntNode() {}
    public IntNode(int tag, Node[] payload) { new Node(tag, payload); }
    public IntNode(int val)
    {
      tag = Runtime.CINT;
      intVal = val;
    }
  }

  public static int CINT;

  // Todo: parametrize these from haskell.
  public static Node[]  RP      = new Node[256];
  public static Node   CRP      = new Node(1, Runtime.RP);
  public static Node[] Globals  = new Node[256];

  public static void initialize (int CInt)
  {
    Runtime.CINT = CInt;
    System.out.println("CInt: " + CInt);

    for (int i = 0; i < 256; i++)
      Runtime.RP[i] = null;
  }

  public static void finish ()
  {
    System.out.println("--- CRP:");

    dumpNode(0, Runtime.CRP, 0);
    // System.out.println(Runtime.CRP.intVal);

    // for (int i = 0; i < 256; i++)
      // dumpNode(i, Runtime.RP[i], 0);

    // System.out.println("--- Globals:");

    // for (int i = 0; i < 256; i++)
      // dumpNode(i, Runtime.Globals[i], 0);

  }

  public static final int MAX_DUMP_LEVEL = 2;

  public static void dumpNode (int i, Node node, int level)
  {
    if (node == null)
      return;

    if (level == 0) {
      Runtime.indent(level);
      System.out.println("[" + i + "]");
    }

    Runtime.indent(level);
    System.out.println("tag = " + node.tag);
    if (node.tag == Runtime.CINT) {
      Runtime.indent(level + 1);
      System.out.println("int = " + ((Runtime.IntNode)node.payload[0]).intVal);
    } else {
      if (level + 1 <= Runtime.MAX_DUMP_LEVEL)
        for (int p = 0; p < node.payload.length; p++)
          dumpNode(i, node.payload[p], level + 1);
      else {
        Runtime.indent(level + 1);
        System.out.println("...");
      }
    }
  }

  public static void indent (int level)
  {
    for (int i = 0; i < level; i++)
      System.out.print("  ");
  }

  static void test ()
  {
    Globals[3] = new Node();
    Globals[3].payload = new Node[3];
    Globals[3].payload[1] = Globals[3];
  }

  static void paramTest (int fst, int snd, int trd)
  {
    System.out.println(fst);
    System.out.println(snd);
    System.out.println(trd);
  }

  public static Node primAddInt(Node l, Node r)
  {
    return new IntNode(((IntNode) l).intVal + ((IntNode) r).intVal);
  }

}

