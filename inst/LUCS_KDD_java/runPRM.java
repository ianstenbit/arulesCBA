import java.io.*;


public class runPRM {

  public static void main(String[] args) throws IOException {

    PRM_CARgen newClassification = new PRM_CARgen(args);

    newClassification.inputDataSet();
    newClassification.outputDataArraySize();

    double accuracy = newClassification.startClassification();
    newClassification.getCurrentRuleListObject().outputNumRules();
    newClassification.getCurrentRuleListObject().outputRules();

    System.exit(0);
  }
}
