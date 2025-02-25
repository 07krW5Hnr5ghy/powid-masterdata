package com.proyect.masterdata.utils;
import org.springframework.core.io.ClassPathResource;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryManager {
    public static void createDirectoryIfNotExists() {
        try {
            // Obtiene la ruta de la carpeta resources
            File resourceDir = new ClassPathResource("uploads/products/").getFile();

            // Crea las carpetas si no existen
            if (!resourceDir.exists()) {
                boolean created = resourceDir.mkdirs();
                if (created) {
                    System.out.println("Carpeta creada: " + resourceDir.getAbsolutePath());
                } else {
                    System.out.println("No se pudo crear la carpeta.");
                }
            } else {
                System.out.println("La carpeta ya existe.");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
