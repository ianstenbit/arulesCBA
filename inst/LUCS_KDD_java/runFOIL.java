import java.io.*;

public class runFOIL {

  public static void main(String[] args) throws IOException {

    FOIL_CARgen newClassification = new FOIL_CARgen(args);

    newClassification.inputDataSet();
    newClassification.outputDataArraySize();

    double accuracy = newClassification.startClassification();
    newClassification.getCurrentRuleListObject().outputNumRules();
    newClassification.getCurrentRuleListObject().outputRules();

    System.exit(0);
  }
}
