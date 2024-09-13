package com.proyect.masterdata.controller;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IReport;
import lombok.AllArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("report")
@AllArgsConstructor
public class ReportController {
    private final IReport iReport;
    @GetMapping("general-stock")
    private ResponseEntity<byte[]> general(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iReport.generalStockReport(user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=inventario_general.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("warehouse-stock")
    private ResponseEntity<byte[]> warehouseStock(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iReport.warehouseStockReport(user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=inventario_almacen.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("daily-sale")
    private ResponseEntity<byte[]> dailySale(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iReport.dailySalesSummary(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=resumen.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }
}
