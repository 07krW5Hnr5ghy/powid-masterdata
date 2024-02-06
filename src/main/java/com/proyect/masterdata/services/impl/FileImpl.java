package com.proyect.masterdata.services.impl;

import com.cloudinary.Cloudinary;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.services.IFile;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class FileImpl implements IFile {

    private final Cloudinary cloudinary;
    @Override
    public String uploadFile(MultipartFile multipartFile, String filename) throws IOException {
        try{
            return cloudinary.uploader()
                    .upload(multipartFile.getBytes(), Map.of(
                            "public_id", filename
                    ))
                    .get("url")
                    .toString();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new IOException(e.getMessage());
        }
    }
}
