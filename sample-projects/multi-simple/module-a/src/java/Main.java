public class Main {
	
	public static void main(String[] args) {
		System.out.println("Hello World!  My Dependency Says: " + us.penrose.b.BStuff.NAME );
		
		Runtime.getRuntime().addShutdownHook(new Thread(){
			public void run(){
				System.out.println("Shutting down ...");
			}
		});
	}
}