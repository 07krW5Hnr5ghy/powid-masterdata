package com.proyect.masterdata.controller;

import com.proyect.masterdata.services.IFile;
import lombok.AllArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static java.nio.file.Files.copy;
import static java.nio.file.Paths.get;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

@RestController
@RequestMapping("file")
@CrossOrigin({"*"})
@AllArgsConstructor
public class FileController {
    public static final String DIRECTORY = System.getProperty("user.home") + "/Downloads/uploads/";
    private final IFile iFile;

//    @PostMapping("upload")
//    public ResponseEntity<List<String>> uploadFiles(
//            @RequestParam("files") List<MultipartFile> multipartFiles
//    ) throws IOException {
//        List<String> filenames = new ArrayList<>();
//        for(MultipartFile file : multipartFiles){
//            String filename = StringUtils.cleanPath(file.getOriginalFilename());
//            Path fileStorage = get(DIRECTORY,filename).toAbsolutePath().normalize();
//            copy(file.getInputStream(),fileStorage,REPLACE_EXISTING);
//            filenames.add(filename);
//        }
//        return new ResponseEntity<>(filenames, HttpStatus.OK);
//    }

//    @PostMapping("upload")
//    public String uploadFile(@RequestParam("image")MultipartFile multipartFile) throws IOException {
//        return iFile.uploadFile(multipartFile);
//    }
    @GetMapping("download/{filename}")
    public ResponseEntity<Resource> downloadFile(
            @PathVariable("filename") String filename
    ) throws IOException {
        Path filePath = get(DIRECTORY).toAbsolutePath().normalize().resolve(filename);

        if(!Files.exists(filePath)){
            throw new FileNotFoundException(filename + "was not found in the server");
        }

        Resource resource = new UrlResource(filePath.toUri());
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.add("File-Name",filename);
        httpHeaders.add(HttpHeaders.CONTENT_DISPOSITION,"attachment;File-Name=" + resource.getFilename());
        return ResponseEntity.ok().contentType(MediaType.parseMediaType(Files.probeContentType(filePath))).headers(httpHeaders).body(resource);
    }
}
