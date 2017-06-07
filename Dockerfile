#Dont use maven directly, because all maven dependencies downloaded during build time won't be in the image
FROM openjdk:8-jdk

RUN apt-get update && apt-get install -y  \
		maven \
		llvm \
		graphviz \
		build-essential \
	&& rm -rf /var/lib/apt/lists/*


WORKDIR /moco

COPY pom.xml /moco/pom.xml
#COPY settings-maven.xml /maven

#Download everything
#... except surefire :-(
RUN mvn dependency:resolve
RUN mvn dependency:resolve-plugins
RUN mvn antlr4:antlr4
RUN mvn test --fail-never

#Add sources last, because they are changed often
COPY src /moco/src

#RUN mvn package

ENTRYPOINT mvn
CMD package