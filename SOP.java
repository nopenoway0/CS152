// Object Language: Sums of Products Language
// Includes order of operation
// Interpretted Language

import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;
/**
 * Implements reading of repl and parse functions
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
				if(t == null) return;
				System.out.println("> " + t.execute());
			}catch(Exception e){
				System.out.println("> Error: " + e);
			}
		}
	}
	/**
	 * Takes input parse the input and stores it into an expression which is then returned
	 * @param  line      data to parse and process
	 * @return           an expression containing all operands properly seperated according to the necessary operation
	 * @throws Exception Any exception that may occur during parsing. Most common is invalid input that can't be converted to a double
	 */
	public Expression parse(String line) throws Exception{
		if(line.compareTo("quit") == 0) return null;
		Scanner tokens = new Scanner(line);
		Scanner tokensp;
		tokens.useDelimiter("\\s*\\+\\s*");
		Sum operands = new Sum();
		double a = 0;
		int index = 0;
		String tmp = "";
		Product p1;
		//One operand short - add boolean to complete full loop
		while(tokens.hasNext()){
			// Keep track of count to decide on which product list - in the list - to add to by index
			tmp = tokens.next();

			// Parse Product
			if(tmp.contains("*")){
				operands.add(new Product());
				tokensp = new Scanner(tmp).useDelimiter("\\s*\\*\\s*");
				while(tokensp.hasNext()){
					tmp = tokensp.next();
					a = Double.parseDouble(tmp);
					operands.add(index, a);
				}
				index++;
			}
			//Parse Sum
			else{
				a = Double.parseDouble(tmp);
				p1 = new Product(a);
				operands.add(p1);
				//System.out.println("Entered: " + a);
				index++;
			}
			//System.out.println("String is: " + tmp);
		}
		return operands;
	}
}

/**
 * Interface to be implemented by Sum and Product class
 */
interface Expression{
	public double execute();
}

class Sum implements Expression{
	private List<Product> operands;

	/**
	 * Add operand to the list
	 * @param a operand to be added
	 */
	public void add(Product a){
		this.operands.add(a);
	}
	/**
	 * Adds a double to a product list, within the list of operands
	 * @param index which product list to change
	 * @param a     what double to insert into the list
	 */
	public void add(int index, double a){
		this.operands.get(index).add(a);
	}

	Sum(){
		// Initialize arraylist
		operands = new ArrayList<Product>();
	}

	/**
	 * Triggers all operands in the list, and there list to call their execute function
	 * @return returns the sum of all products
	 */
	public double execute(){
		double sum = 0;
		for(Product a : operands){
			sum += a.execute();
		}
		return sum;
	}
}

class Product implements Expression{
	private List<Double> operands;

	Product(){
		operands = new ArrayList<Double>();
	}

	Product(double a){
		this();
		this.operands.add(a);
	}
	// Adds a double to the list of operands
	public void add(double a){
		this.operands.add(a);
	}

	// Multiplies all operands in the list and returns the product
	public double execute(){
		double product = 1;
		for(double a:this.operands){
			product *= a;
		}
		return product;
	}
}