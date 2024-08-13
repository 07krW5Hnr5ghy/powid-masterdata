package com.proyect.masterdata.controller;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPdfGenerator;
import lombok.AllArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.InputStream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("pdf")
@AllArgsConstructor
public class PdfController {
    private final IPdfGenerator iPdfGenerator;
    @PostMapping()
    public ResponseEntity<InputStreamResource> generatePdf() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<InputStream> pdfStream = iPdfGenerator.generate();
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition","inline;filename=generated.pdf");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_PDF)
                .body(new InputStreamResource(pdfStream.get()));
    }
}
