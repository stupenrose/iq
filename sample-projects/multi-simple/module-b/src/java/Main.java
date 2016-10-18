public class Main {
	
	public static void main(String[] args) {
		System.out.println("Hello World!  This is my first IQ build!");
		
		Runtime.getRuntime().addShutdownHook(new Thread(){
			public void run(){
				System.out.println("Shutting down ...");
			}
		});
	}
}