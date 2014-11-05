[![Build Status](https://travis-ci.org/MontysCoconut/moco.svg?branch=master)](https://travis-ci.org/MontysCoconut/moco)

Allgemeines
-----------

Dieses Projekt setzt Java in der Version 7 ein. Stellt sicher, dass ihr keine
Features von Java 8 verwendet. Die Referenz ob das Projekt baut und die Tests
laufen sind die Maven-Runs.

Es wird durch Maven automatisch ein Code-Formatter ausgeführt. Bitte
kompiliert also vor dem commiten einmal mit Maven oder konfiguriert eure IDE,
sodass diese den Code entsprechend formatiert.

Zu installierende Software:

- Java 7
- Maven 3 http://maven.apache.org/
- LLVM 3.4 http://llvm.org/
- Graphviz http://www.graphviz.org/

Bei Windows benötigt ihr extra noch:

- ein 32bit JDK
- Visual C++ Redistributable Packages für Visual Studio 2013
    http://www.microsoft.com/de-de/download/details.aspx?id=40784

Kompilation und Ausführen
-------------------------

- `mvn package`
    Generiert die nötigen ANTLR4 Dateien, kompiliert alle wichtigen Dateien,
    führt die Tests aus und baut ein ausführbares JAR
    (`target/moco-0.6.jar`).
- `mvn package -Dmaven.test.skip=true`
    Führt die Tests nicht aus.
Habt ihr das JAR erstellt, könnt ihr die Main Klasse mit `java -jar
target/moco-0.6.jar` starten.

Maven Reporting
---------------

Maven wurde eingerichtet Reports zu generieren.

`mvn site` erstellt die Dokumentation und generiert Reports unter
[target/site/](project-reports.html). Besonders interessant:

 - [JavaDoc](apidocs/index.html)
 - [Code Coverage Analysis](cobertura/index.html)

Außerdem gibt es Analysen auf dem Code. Bitte beachtet, dass euer Code nicht
zu viele Einträge in folgenden Reports erzeugt:

- [Checkstyle](checkstyle.html)
- [Findbugs](findbugs.html)

Entwicklungsumgebungen
----------------------

###Eclipse:

- Maven kann mit `mvn eclipse:eclipse` Eclipse Konfiguration generieren.
    Ist dies erledigt kann man das Projekt einfach als existierendes Projekt
    importieren.
- Es wird ein Code Formatter verwendet. Unter
    `src/main/resources/java-code-conv.xml` findet sich ein in Eclipse
    importierbares Profil.
- Alternativ:
    - m2e in Eclipse installieren: http://download.eclipse.org/technology/m2e/releases
    - File -> Import -> Maven -> Existing Maven Projects -> Browse... -> Auf Projekt Zeigen -> Next -> Finish -> OK
    - Rechtsklick auf Projekt moco -> Maven -> Update Project Config...
    - Ordner "target" -> generated-sources -> Rechtsklick auf "antlr4" -> Build Path -> Use as source Folder
    - Run
