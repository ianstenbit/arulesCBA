import java.io.*;

public class runCPAR {

  public static void main(String[] args) throws IOException {

    CPAR_CARgen newClassification = new CPAR_CARgen(args);

    newClassification.inputDataSet();
    newClassification.outputDataArraySize();

    double accuracy = newClassification.startClassification();
    newClassification.getCurrentRuleListObject().outputNumRules();
    newClassification.getCurrentRuleListObject().outputRules();

    System.exit(0);
  }
}
