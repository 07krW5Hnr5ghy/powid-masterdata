package com.proyect.masterdata.utils;

import com.proyect.masterdata.services.IAudit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.zip.Deflater;


public class CompressionString {

    private static final Logger logger = LoggerFactory.getLogger(CompressionString.class);

    /**
     * Comprime una cadena en Base64 utilizando el algoritmo Deflate.
     *
     * @param data Cadena a comprimir.
     * @return Cadena comprimida en Base64.
     * @throws IllegalArgumentException si los datos no se pueden comprimir.
     */
    public static String compressStringToBase64(String data) {
        if(data == null || data.isEmpty()){
            throw new IllegalArgumentException("data is null or empty");
        }
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            Deflater deflater = new Deflater(Deflater.BEST_COMPRESSION);
            deflater.setInput(data.getBytes(StandardCharsets.UTF_8));
            deflater.finish();
            byte[] buffer = new byte[1024];
            while (!deflater.finished()){
                int count = deflater.deflate(buffer);
                outputStream.write(buffer,0,count);
            }
            deflater.end();
            String compressedBase64 = Base64.getEncoder().encodeToString(outputStream.toByteArray());
            logger.debug("Compressed Base64 string successful: {}", compressedBase64);

            return  compressedBase64;
        }catch (Exception e){
            logger.debug("Error compressing data", e);
            throw new IllegalArgumentException("Error compressing data. ", e);
        }
    }

}
