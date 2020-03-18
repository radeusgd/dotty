package dotty.tools.dotc.sbt;

import java.nio.file.Path;

public interface APICallback {

  interface DefinitionType {
    int TRAIT = 0;
    int CLASS_DEF = 1;
    int MODULE = 2;
    int PACKAGE_MODULE = 3;
  }

  default void startSource(Path src) {}
  default void endSource() {}

  default void startClassLikeDef(int definitionType, String name) {}
  default void endClassLikeDef() {}

  default void startVal(String name) {}
  default void endVal() {}

  default void startDef(String name) {}
  default void endDef() {}

  default void startParamList(boolean isImplicit) {}
  default void endParamList() {}

  default void startProjection(String selected) {}
  default void endProjection() {}

  default void startConstant(Object constant) {}
  default void endConstant() {}

  default void startSingleton() {}
  default void endSingleton() {}

  default void startPath() {}
  default void endPath() {}

  default void id(String name) {}
  default void thisId() {}
  default void publicAPI() {}
  default void modifiers(
    boolean isAbstract,
    boolean isOverride,
    boolean isFinal,
    boolean isSealed,
    boolean isImplicit,
    boolean isLazy,
    boolean isMacro,
    boolean isSuperAccessor
  ) {}

}
