package hudson.util;

import hudson.util.ProcessTree.OSProcess;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;

public class Main {
	public static void main(String[] args) throws Exception {
		
		BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
		
		while(true){
			System.out.println("Starting");
			Process p = new ProcessBuilder("./run.sh").directory(new File("/home/stu/Desktop/foo")).start();
			System.out.println("started");

			r.readLine();
			System.out.println("Killing");
			OSProcess op = ProcessTree.get().get(p);
			op.killRecursively();
		}
	}
}
