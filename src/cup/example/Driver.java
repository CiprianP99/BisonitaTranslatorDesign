package cup.example;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java_cup.runtime.*;

class Driver {

	public static void main(String[] args) throws Exception {
		ComplexSymbolFactory f = new ComplexSymbolFactory();
		File file = new File("input.txt");
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(file);
		} catch (IOException e) {
			e.printStackTrace();
		}
		Lexer lexer = new Lexer(f,fis);
		Symbol currentSymbol;
		
		do
		{
			currentSymbol = lexer.next_token();
			if(currentSymbol != null)
			{
				System.out.println("Token: "+currentSymbol.toString());
			}
		}while(currentSymbol != null && currentSymbol.sym != sym.EOF);
	}
	
}