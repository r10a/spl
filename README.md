# Compiler Construction
## CSE-5317 Design and Construction of Compilers academic project
<hr>

Phase 1: Implement a Scanner using the SPL language specification.  
Phase 2: Implement a Parser which uses the Scanner from the previous phase.  
Phase 3: Implement an AST generator using the Scanner output.  
Phase 4: Implement Type Checking in the generated AST.  
Phase 5: Implement Intermediate Code generation from the generated AST.  
Phase 6: Implement Machine code generation for the MIPS instrucution set.  


### Instructions
Platform and Tools
- If you do not have Java on your PC, install Java SE 8 JDK.
- Setup the JAVA_HOME variable. On Microsoft Windows 8/10, click Start, then Control Panel, then System, then Advanced, then Environment Variables. Insert a new System variable JAVA_HOME to be C:\Java\jdk1.8.0_111 (change the version to match).
- Install Apache maven from https://maven.apache.org/install.html.
- Install Scala 2.11.* from http://www.scala-lang.org/download/ (do not install Scala 2.12.*).
- Change your PATH. On Microsoft Windows 8/10, click Start, then Control Panel, then System, then Advanced, then Environment Variables. Edit the System variable Path and insert C:\Java\jdk1.8.0_111\bin, C:\apache-maven-3.3.9\bin, and C:\Program Files (x86)\scala\bin at the top (change the versions to match).

### Run
Build using -   
``mvn clean install``  
Test code using -   
``scala lib/spl.jar <compiler-phase> tests/hello.spl``  
eg. ``scala lib/spl.jar 1 tests/hello.spl``  
Check solution -  
``scala spl-solution.jar <compiler-phase> tests/hello.spl``  
eg. ``scala spl-solution.jar 1 tests/hello.spl``  
