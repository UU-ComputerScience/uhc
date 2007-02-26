import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

class Hi extends JFrame
{
	JTextArea outputArea;
	JLabel   statusLabel;
	JTextField inputField;
	StdIOProcess process;

	Hi()
	{
		super("hi");
		setSize(800, 600);

		inputField = new JTextField(60);
		inputField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				statusLabel.setText("Handle user input");
				handleUserInput(inputField.getText());
			}});

/*
		interruptButton = new Button("Interrupt");
		interruptButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				interruptButton.setEnabled(false);
				statusLabel.setText("Interrupted by user (please wait)");
				process.destroy();
				while (!done)
					try { Thread.sleep(100); } catch (InterruptedException ie) {}
				statusLabel.setText("Done");
			}});
*/

	 	LogoCanvas logoCanvas = new LogoCanvas();

		final JTextField runtimeInput = new JTextField(60);
		runtimeInput.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				if (process != null)
					process.sendInput(runtimeInput.getText());
			}});

		JPanel northWestPanel = new JPanel();
		northWestPanel.setLayout(new GridLayout(2,2));
		northWestPanel.add(new JLabel("Expression: "));
		northWestPanel.add(inputField);
//		northWestPanel.add(interruptButton);
		northWestPanel.add(new JLabel("Runtime input: "));
		northWestPanel.add(runtimeInput);

		JPanel northPanel = new JPanel();
		northPanel.setLayout(new GridLayout(2,1));
		northPanel.add(northWestPanel);
		northPanel.add(logoCanvas);

		outputArea = new JTextArea(25, 80);
		outputArea.setFont(new Font("Monospaced", Font.PLAIN, 12));
		outputArea.setEditable(false);

		outputArea.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent me) {
				try {
					String file = "unknown";
					int line = outputArea.getLineOfOffset(outputArea.getCaretPosition());
					String text = outputArea.getText();
					StringTokenizer tokenizer = new StringTokenizer(text, "\n\r");
					while (tokenizer.hasMoreTokens() && line > 0) {
						String lineText = tokenizer.nextToken();
						System.out.println(line + "***" + lineText + "***");
						if (lineText.startsWith("Compiling"))
							file = lineText.substring("Compiling".length() + 1);
						line--;
					}
					if (line == 0 && tokenizer.hasMoreTokens()) {
						String lineText = tokenizer.nextToken();
						if (lineText.charAt(0) == '(') {
							int close = lineText.indexOf(')');
							if (close >= 0) {
								String position = lineText.substring(0, close+1);
								System.out.println("position = " + position);
								System.out.println("file     = " + file);
								Runtime runtime = Runtime.getRuntime();
								try {
									runtime.exec("C:\\apps\\TextPad\\TextPad.exe " + file + position);
								} catch (Exception e) {}
							}

						}

					}
				} catch (BadLocationException ble) {}
			}});

		JScrollPane outputScroll = new JScrollPane(outputArea);

		statusLabel = new JLabel("Waiting for input");

		Container content = getContentPane();
		content.setLayout(new BorderLayout());
		content.add(northPanel, BorderLayout.NORTH);
		content.add(outputScroll, BorderLayout.CENTER);
		content.add(statusLabel, BorderLayout.SOUTH);

		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent we) {
				System.exit(0);
			}});

		setVisible(true);
	}

	public static void main(String args[])
	{
		try
		{
			new Hi();
		} catch (Exception e)
		{
			System.out.println("Uncaught exception: " + e);
			e.printStackTrace();
			System.exit(0);
		}
	}

	private final static String HELIUM_INPUT_MODULE = "HeliumInput";
	private final static String HELIUM_INPUT_FILE = HELIUM_INPUT_MODULE + ".hs";
	private final static String LVM_FILE          = HELIUM_INPUT_MODULE + ".lvm";
	private final static String HELIUM_OUTPUT_FILE = "HeliumOutput.txt";
	private final static String MAIN_FUNCTION = "interpreter_main";

	void handleUserInput(String input)
	{
		outputArea.append(">>> " + input + "\n");

		createModule(input);
		if (runCompiler())
			runGeneratedCode();
	}

	void createModule(String input)
	{
		statusLabel.setText("Creating temporary file");

		try
		{
			PrintWriter printWriter =
				new PrintWriter(new FileWriter(HELIUM_INPUT_FILE));
			printWriter.println("module " + HELIUM_INPUT_MODULE + " where");
			printWriter.println(MAIN_FUNCTION + " = " + input);
			printWriter.close();
		} catch (IOException ie)
		{
			System.out.println("Failed to write to " + HELIUM_INPUT_FILE + "\nReason: " + ie);
			System.exit(1);
		}
	}

	boolean runCompiler()
	{
		Runtime runtime = Runtime.getRuntime();
		String command = "helium " + HELIUM_INPUT_FILE;

		statusLabel.setText("Starting Helium compiler");
		System.out.println("Starting Helium compiler " + command);

		Process process = null;
		BufferedInputStream stdout = null;
		try {
			process = runtime.exec(command);
			stdout  = new BufferedInputStream(process.getInputStream());
		} catch (IOException ie) {
			System.out.println("Exception while executing command: " + command +
								"\nReason: " + ie);
			System.exit(1);
		}

		System.out.println("Capturing output of Helium compiler");
		statusLabel.setText("Capturing output of Helium compiler");

		StringBuffer output = new StringBuffer();
		int ch;

		try {
			while ((ch = stdout.read()) != -1)
				output.append((char) ch);
		} catch (IOException ie) {
			System.out.println("Exception while capturing output of compiler\n" +
								"Reason: " + ie);
			System.exit(1);
		}


		System.out.println("Waiting for Helium compiler");
		statusLabel.setText("Waiting for Helium compiler");

		try {
			process.waitFor();
		} catch (InterruptedException ie) {
			System.out.println("Exception while waiting for command: " + command +
								"\nReason: " + ie);
			System.exit(1);
 		}

		System.out.println("Showing output of Helium compiler");
		statusLabel.setText("Showing output of Helium compiler");

		outputArea.append(output.toString());

		if (process.exitValue() != 0) {
			statusLabel.setText("Compilation failed");
			return false;
		} else {
			statusLabel.setText("Compilation successful");
			return true;
		}
	}

	void runGeneratedCode()
	{
		statusLabel.setText("Running generated code");
		process = new StdIOProcess("lvmrun " + LVM_FILE, outputArea);
		outputArea.append("\n");
	}

/*
	void setExecuting(boolean executing)
	{
		interruptButton.setEnabled(executing);
		inputField.setEditable(!executing);
		done = !executing;
	}
*/

}

class LogoCanvas extends Canvas
{
	 Image logoImage;

		LogoCanvas()
		{
			setSize(150, 45);
			logoImage = Toolkit.getDefaultToolkit().getImage("Logo.jpg");
			MediaTracker tracker = new MediaTracker(this);
			tracker.addImage(logoImage, 0);
			try {
				tracker.waitForAll();
			} catch (Exception e) {
				System.exit(0);
			}
	}

	public void paint(Graphics g)
	{
		g.drawImage(logoImage, 0, 0, null);
	}

}

class StdIOProcess implements Runnable
{
	JTextArea outputArea;

	Process process;
	PrintWriter stdin;
	BufferedInputStream stdout;

//	private final static int OUTPUT_WIDTH = 80;
//	int count = 0;

	StdIOProcess(String command, JTextArea outputArea)
	{
		Runtime runtime = Runtime.getRuntime();
		try {
			process = runtime.exec(command);
		} catch (IOException ie) {
			System.out.println("StdIOProcess.StdIOProcess: " + ie);
			System.exit(1);
		}

		this.outputArea = outputArea;
		stdin = new PrintWriter(process.getOutputStream(), true);
		stdout = new BufferedInputStream(process.getInputStream());
		Thread thread = new Thread(this);
		thread.start();
	}

	void sendInput(String s)
	{
		outputArea.append(s + "\n");
		stdin.println(s);
//		count = 0;
	}

	public void run()
	{
		int ch;
		try {
			while ((ch = stdout.read()) != -1) {
				outputArea.append("" + (char) ch);
/*				if (ch == 10 || ch == 13)
					count = 0;
				else {
					count++;
					if (count % OUTPUT_WIDTH == 0) { outputArea.append("\n"); count = 0; }
				}
*/
			}
		} catch (IOException ie) {
			System.out.println("StdIOProcess.run: " + ie);
			System.exit(1);
		}
	}

	void waitUntilDone()
	{
		try {
			process.waitFor();
		} catch (InterruptedException ie) {
			System.out.println("StdIOProcess.waitUntilDone: " + ie);
			System.exit(1);
		}
	}

	int exitValue()
	{
		return process.exitValue();
	}

}
