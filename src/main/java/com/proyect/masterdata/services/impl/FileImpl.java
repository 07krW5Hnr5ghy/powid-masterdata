package com.proyect.masterdata.services.impl;

import com.cloudinary.Cloudinary;
import com.cloudinary.Transformation;
import com.proyect.masterdata.services.IFile;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class FileImpl implements IFile {

    private final Cloudinary cloudinary;
    @Override
    public CompletableFuture<String> uploadFile(MultipartFile multipartFile, String filePath) throws IOException {
        return CompletableFuture.supplyAsync(()->{
            try{
                return cloudinary.uploader()
                        .upload(multipartFile.getBytes(), Map.of(
                                "public_id", filePath,
                                "transformation",new Transformation<>().quality("auto")
                        ))
                        .get("url")
                        .toString();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                try {
                    throw new IOException(e.getMessage());
                } catch (IOException ex) {
                    throw new RuntimeException(ex);
                }
            }
        });
    }
}
