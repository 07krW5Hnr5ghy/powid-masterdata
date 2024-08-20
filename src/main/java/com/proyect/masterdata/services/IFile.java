package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.User;
import org.springframework.web.multipart.MultipartFile;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.CompletableFuture;

public interface IFile {
    CompletableFuture<String> uploadFile(MultipartFile multipartFile, String filePath) throws IOException;
    CompletableFuture<String> uploadFiles(File file,String filePath) throws IOException;
}
