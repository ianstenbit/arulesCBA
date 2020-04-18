import java.io.*;

// specify min support and confidence: -S5 -C80

public class runCMAR {

    public static void main(String[] args) throws IOException {
		AprioriTFP_CMAR newClassification = new AprioriTFP_CMAR(args);
		newClassification.inputDataSet();
		newClassification.idInputDataOrdering();  
		newClassification.recastInputData();    
	
		newClassification.startClassification();
	 
		newClassification.outputNumFreqSets();
		newClassification.outputNumUpdates();
		newClassification.outputStorage();
		newClassification.outputNumCMARrules();
		newClassification.outputCMARrules();
	
		System.exit(0);
		}
		
}
