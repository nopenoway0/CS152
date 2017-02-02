// Object Language: Sums of Products Language
// Includes order of operation
// Interpretted Language

import java.util.Scanner;
import java.util.List;

/**
 * Implements reading of input and endless loop
 */
class Console{
	public static void main(String[] args){
		Console main_run = new Console();
		main_run.repl();
		return;
	}

	Console(){

	}

	public void repl(){
		Expression t = null;
		while(true){
			System.out.print("->");
			Scanner input = new Scanner(System.in);
			String entered_input = input.nextLine();
			try{
				t = parse(entered_input);
			}catch(Exception e){
				System.out.println("Error: " + e);
			}
			if(t == null) return;
		}
	}

	public Expression parse(String line) throws Exception{
		if(line.compareTo("quit") == 0) return null;
		Scanner tokens = new Scanner(line);
		tokens.useDelimiter("\\s*\\+\\s*");
		Sum operands = new Sum();
		double a;
		double b;
		//One operand short - add boolean to complete full loop
		while(tokens.hasNext()){
			a = Double.parseDouble(tokens.next());
		}
		return null;
	}
}

interface Expression{
	public double execute();
}

class Sum implements Expression{
	private List<Product> operands;

	public void addProduct(Product a){
		this.operands.add(a);
	}

	public void addOperand(int index, double a){
		this.operands.get(index).addOperand(a);
	}

	Sum(){

	}
	public double execute(){
		return 1;
	}
}

class Product implements Expression{
	private List<Double> operands;

	public void addOperand(double a){
		this.operands.add(a);
	}

	public double execute(){
		return 2;
	}
}