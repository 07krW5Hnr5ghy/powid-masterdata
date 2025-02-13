FROM openjdk:17-jdk-slim
ARG JAR_FILE=target/masterdata-0.0.1-SNAPSHOT.jar
COPY ${JAR_FILE} masterdata_backend.jar
EXPOSE 8080
ENTRYPOINT ["java","-jar","masterdata_backend.jar"]