package com.proyect.masterdata.services.impl;

import com.cloudinary.Cloudinary;
import com.cloudinary.Transformation;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IFile;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
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
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<String> uploadFiles(File file, String filePath) throws IOException {
        return CompletableFuture.supplyAsync(()->{
            try{
                byte[] fileBytes = Files.readAllBytes(file.toPath());
                return cloudinary.uploader()
                        .upload(fileBytes, Map.of(
                                "public_id", filePath,
                                "transformation",new Transformation<>().quality("auto")
                        ))
                        .get("url")
                        .toString();
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
