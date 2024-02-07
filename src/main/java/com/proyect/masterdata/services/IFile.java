package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.User;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public interface IFile {
    String uploadFile(MultipartFile multipartFile, String filePath) throws IOException;
}
