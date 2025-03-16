package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestPdfOrder;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPdfGenerator;
import lombok.AllArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.InputStream;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("pdf")
@AllArgsConstructor
public class PdfController {
    private final IPdfGenerator iPdfGenerator;
    @PostMapping("order")
    public ResponseEntity<InputStreamResource> generateOrder(
            @RequestBody()RequestPdfOrder requestPdfOrder
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<InputStream> pdfStream = iPdfGenerator.generateOrderReport(requestPdfOrder.getOrderId(), requestPdfOrder.getTokenUser());
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition","inline;filename=pedido_"+requestPdfOrder.getOrderId()+".pdf");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_PDF)
                .body(new InputStreamResource(pdfStream.get()));
    }

    @PostMapping("delivery-manifest")
    public ResponseEntity<InputStreamResource> generateDeliveryManifest(
            @RequestParam("deliveryManifestId") String deliveryManifestId,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<InputStream> pdfStream = iPdfGenerator.generateDeliveryManifestReport(deliveryManifestId, tokenUser);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition","inline;filename=pedido_"+deliveryManifestId+".pdf");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_PDF)
                .body(new InputStreamResource(pdfStream.get()));
    }
}
