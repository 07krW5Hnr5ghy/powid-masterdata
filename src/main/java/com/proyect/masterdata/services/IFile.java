package com.proyect.masterdata.services;

import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public interface IFile {
    String uploadFile(MultipartFile multipartFile) throws IOException;
}
